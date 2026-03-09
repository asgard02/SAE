"""
CEM Rennes – Enrichisseur de données réelles
=============================================
Lit le vrai fichier CEM_anonymized (14 colonnes réelles)
et y ajoute les colonnes fictives nécessaires au dashboard R Shiny.

Colonnes RÉELLES du fichier CEM (conservées telles quelles) :
    id, sexe, date_birth, type_sejour, date_cre_sej, date_sej,
    date_last_visit, date_death, nb_doc, type_pec,
    diag_cancer, diag_associe, diag_meta, diag_antecedents

Colonnes GÉNÉRÉES (fictives, cohérentes cliniquement) :
    date_discharge   → date_sej + durée selon type de séjour
    date_last_vis    → alias de date_last_visit (pour compatibilité global.R)
    stade_tnm        → I/II/III/IV selon diag_meta et diag_cancer
    score_oms        → 0-4 selon stade
    voie_administration → selon type_sejour
    protocole        → selon type_sejour
    nb_cures         → selon type_sejour
    imc              → selon diag_cancer
    tabac_paquet_an  → selon diag_cancer et antécédents
    code_postal, ville, departement → Bretagne
    niveau_education, situation_familiale → socio-démographique

Usage :
    python3 enrich_csv.py --input patients_data.csv --output patients_data_enrichi.csv

    Si le fichier est en UTF-8 avec BOM (export Excel) :
    python3 enrich_csv.py --input patients_data.csv --output patients_data_enrichi.csv --encoding utf-8-sig

    Si les colonnes sont séparées par des ";" :
    python3 enrich_csv.py --input patients_data.csv --output patients_data_enrichi.csv --sep ";"
"""

import csv
import os
import random
import argparse
import sys
from datetime import datetime, date, timedelta
from typing import Dict, Optional

random.seed(42)

# ──────────────────────────────────────────────
# COLONNES DE SORTIE (ordre final pour le CSV)
# ──────────────────────────────────────────────
OUTPUT_COLUMNS = [
    # Réelles (conservées)
    "id", "sexe", "date_birth",
    "type_sejour",          # E/H/R/S/?
    "date_cre_sej",         # date de contact (= date_diagnosis dans global.R)
    "date_sej",             # date d'entrée
    "date_discharge",       # date de sortie (GÉNÉRÉE)
    "date_last_vis",        # alias de date_last_visit
    "date_death",
    "nb_doc",
    "type_pec",             # 1/2/3/4 (global.R mappe vers labels)
    "diag_cancer",          # CIM-10 (ex: C509)
    "diag_associe",         # CIM-10 comorbidité
    "diag_meta",            # "meta" ou vide
    "diag_antecedents",     # Z85 ou vide
    # Fictives (générées)
    "stade_tnm",
    "score_oms",
    "voie_administration",
    "protocole",
    "nb_cures",
    "imc",
    "tabac_paquet_an",
    "code_postal",
    "ville",
    "departement",
    "niveau_education",
    "situation_familiale",
]

# ──────────────────────────────────────────────
# TABLES DE RÉFÉRENCE
# ──────────────────────────────────────────────

LIEUX_BRETAGNE = [
    ("Rennes",       "35000", "Ille-et-Vilaine (35)", 0.28),
    ("Saint-Malo",   "35400", "Ille-et-Vilaine (35)", 0.06),
    ("Fougères",     "35300", "Ille-et-Vilaine (35)", 0.04),
    ("Vitré",        "35500", "Ille-et-Vilaine (35)", 0.03),
    ("Redon",        "35600", "Ille-et-Vilaine (35)", 0.03),
    ("Brest",        "29200", "Finistère (29)",        0.14),
    ("Quimper",      "29000", "Finistère (29)",        0.08),
    ("Morlaix",      "29600", "Finistère (29)",        0.03),
    ("Concarneau",   "29900", "Finistère (29)",        0.02),
    ("Vannes",       "56000", "Morbihan (56)",         0.09),
    ("Lorient",      "56100", "Morbihan (56)",         0.07),
    ("Pontivy",      "56300", "Morbihan (56)",         0.02),
    ("Saint-Brieuc", "22000", "Côtes-d'Armor (22)",   0.06),
    ("Lannion",      "22300", "Côtes-d'Armor (22)",   0.02),
    ("Dinan",        "22100", "Côtes-d'Armor (22)",   0.03),
]
_LIEUX_POIDS = [l[3] for l in LIEUX_BRETAGNE]

NIVEAUX_EDUCATION     = [("Primaire", 0.18), ("Secondaire", 0.45), ("Universitaire", 0.37)]
SITUATIONS_FAMILIALES = [("Célibataire", 0.22), ("Marié(e)", 0.48), ("Divorcé(e)", 0.15), ("Veuf/Veuve", 0.15)]

# Durée de séjour en jours selon le type
DUREE_PAR_TYPE = {
    "H": (7, 5),    # Hospitalisation : moyenne 7j, écart-type 5
    "R": (0, 0),    # Radiothérapie séance : même jour
    "S": (0, 0),    # Chimiothérapie séance : même jour
    "E": (0, 0),    # Externe : même jour
    "?": (1, 2),    # Inconnu : court
}

# Protocoles selon le type de séjour
PROTOCOLES_PAR_TYPE = {
    "S": ["FOLFOX", "FOLFIRI", "AC-T", "BEP", "CHOP", "R-CHOP", "Gemcitabine", "Carboplatine", "Taxol"],
    "R": ["IMRT", "SBRT", "3D-CRT", "Protonthérapie", "Curiethérapie"],
    "H": ["Résection", "Mastectomie", "Prostatectomie", "Lobectomie", "Colectomie",
          "Morphine IV", "Surveillance post-op", "Bilan biologique"],
    "E": ["Consultation initiale", "Avis spécialisé", "RCP", "Scanner de contrôle",
          "Bilan biologique", "Échographie"],
    "?": ["Non spécifié"],
}

# Voie d'administration selon le type de séjour
VOIES_PAR_TYPE = {
    "S": [("IV (Intraveineuse)", 0.70), ("Orale", 0.25), ("SC (Sous-cutanée)", 0.05)],
    "R": [("Externe", 1.00)],
    "H": [("IV (Intraveineuse)", 0.50), ("Orale", 0.30), ("Chirurgicale", 0.20)],
    "E": [("Non applicable", 1.00)],
    "?": [("Non applicable", 1.00)],
}

# Préfixes CIM-10 → groupe de cancer (pour logique stade/IMC)
def get_groupe_cancer(diag: str) -> str:
    if not diag:
        return ""
    d = diag.upper().strip()
    if d.startswith("C50") or d.startswith("D05"):
        return "sein"
    if any(d.startswith(p) for p in
           ["C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26"]):
        return "digestif"
    if d.startswith("C61"):
        return "prostate"
    if d.startswith("C43") or d.startswith("C44") or d.startswith("D03") or d.startswith("D04"):
        return "peau"
    if d.startswith("C34"):
        return "poumon"
    if d.startswith("C"):
        return "autre_cancer"
    return ""

# Stades selon le groupe et le statut métastatique
STADES_SELON_META = {
    # Si meta → stade III ou IV
    True: [("I", 0.05), ("II", 0.10), ("III", 0.30), ("IV", 0.55)],
    # Si pas meta → stade I ou II
    False: [("I", 0.30), ("II", 0.40), ("III", 0.20), ("IV", 0.10)],
}

# Stades spécifiques par groupe (sans meta)
STADES_PAR_GROUPE = {
    "sein":     [("I", 0.30), ("II", 0.35), ("III", 0.20), ("IV", 0.15)],
    "poumon":   [("I", 0.15), ("II", 0.20), ("III", 0.30), ("IV", 0.35)],
    "digestif": [("I", 0.15), ("II", 0.20), ("III", 0.30), ("IV", 0.35)],
    "prostate": [("I", 0.25), ("II", 0.35), ("III", 0.25), ("IV", 0.15)],
    "peau":     [("I", 0.40), ("II", 0.30), ("III", 0.20), ("IV", 0.10)],
}


def choix_pondere(options):
    """Sélection pondérée depuis une liste de (valeur, poids)."""
    labels, poids = zip(*options)
    return random.choices(labels, weights=poids, k=1)[0]


def parse_date(s: str) -> Optional[date]:
    """Parse une date au format jj/mm/aaaa → objet date Python."""
    if not s or not s.strip():
        return None
    for fmt in ("%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y"):
        try:
            return datetime.strptime(s.strip(), fmt).date()
        except ValueError:
            continue
    return None


def format_date(d: Optional[date]) -> str:
    """Formate une date en jj/mm/aaaa."""
    if d is None:
        return ""
    return d.strftime("%d/%m/%Y")


# ──────────────────────────────────────────────
# CACHE PAR PATIENT
# Garantit cohérence entre séjours d'un même patient
# ──────────────────────────────────────────────
_patient_cache: Dict[str, dict] = {}

def get_patient_data(patient_id: str, diag_cancer: str, is_meta: bool) -> dict:
    """
    Génère (ou récupère du cache) les données fixes du patient.
    Même patient → mêmes valeurs d'un séjour à l'autre.
    """
    if patient_id in _patient_cache:
        return _patient_cache[patient_id]

    groupe = get_groupe_cancer(diag_cancer)

    # Stade TNM : cohérent avec le statut métastatique
    if diag_cancer:
        if is_meta:
            stades = STADES_SELON_META[True]
        else:
            stades = STADES_PAR_GROUPE.get(groupe, STADES_SELON_META[False])
        stade_tnm = choix_pondere(stades)
    else:
        stade_tnm = ""  # pas de diag cancer → pas de stade

    # Score OMS selon le stade
    if stade_tnm == "IV":
        score_oms = random.choices([0,1,2,3,4], weights=[0.05,0.15,0.30,0.35,0.15])[0]
    elif stade_tnm == "III":
        score_oms = random.choices([0,1,2,3,4], weights=[0.10,0.30,0.35,0.20,0.05])[0]
    elif stade_tnm in ("I","II"):
        score_oms = random.choices([0,1,2,3,4], weights=[0.35,0.35,0.20,0.08,0.02])[0]
    else:
        score_oms = random.choices([0,1,2,3,4], weights=[0.25,0.35,0.25,0.10,0.05])[0]

    # IMC : corrélé au groupe cancer
    if groupe in ("poumon", "digestif"):
        # Cachexie fréquente → IMC bas
        imc = round(max(15.0, random.gauss(21, 3)), 1)
    elif groupe == "peau":
        imc = round(max(16.0, min(40.0, random.gauss(26, 4))), 1)
    else:
        imc = round(max(16.0, min(45.0, random.gauss(25, 4))), 1)

    # Tabac : corrélé au groupe cancer
    if groupe in ("poumon",):
        tabac_pa = round(max(5.0, random.gauss(28, 12)), 1)
    elif groupe in ("digestif", "peau"):
        tabac_pa = round(random.choices([0.0, random.uniform(5, 25)], weights=[0.5, 0.5])[0], 1)
    else:
        tabac_pa = round(random.choices([0.0, random.uniform(1, 5)], weights=[0.75, 0.25])[0], 1)

    # Géographie Bretagne
    idx = random.choices(range(len(LIEUX_BRETAGNE)), weights=_LIEUX_POIDS)[0]
    ville, code_postal, departement, _ = LIEUX_BRETAGNE[idx]

    pdata = {
        "stade_tnm":           stade_tnm,
        "score_oms":           str(score_oms),
        "imc":                 str(imc),
        "tabac_paquet_an":     str(tabac_pa),
        "ville":               ville,
        "code_postal":         code_postal,
        "departement":         departement,
        "niveau_education":    choix_pondere(NIVEAUX_EDUCATION),
        "situation_familiale": choix_pondere(SITUATIONS_FAMILIALES),
    }
    _patient_cache[patient_id] = pdata
    return pdata


def enrich_row(row: dict, real_headers: list) -> dict:
    """
    Prend une ligne du vrai CSV CEM et retourne une ligne enrichie.
    """
    # ── Lecture directe des colonnes réelles ──────────────
    patient_id   = row.get("id",               "").strip()
    sexe         = row.get("sexe",             "").strip()
    date_birth   = row.get("date_birth",       "").strip()
    type_sejour  = row.get("type_sejour",      "").strip().upper()
    date_cre_sej = row.get("date_cre_sej",     "").strip()
    date_sej     = row.get("date_sej",         "").strip()

    # date_last_visit (peut s'appeler date_last_visit ou date_last_vis)
    date_last    = (row.get("date_last_visit", "") or
                    row.get("date_last_vis",   "")).strip()

    date_death   = row.get("date_death",       "").strip()
    nb_doc       = row.get("nb_doc",           "").strip()
    type_pec     = row.get("type_pec",         "").strip()
    diag_cancer  = row.get("diag_cancer",      "").strip()
    diag_associe = row.get("diag_associe",     "").strip()
    diag_meta    = row.get("diag_meta",        "").strip()
    diag_antec   = row.get("diag_antecedents", "").strip()

    # ── Statut métastatique ────────────────────────────────
    is_meta = diag_meta.lower() == "meta"

    # ── Données fixes par patient (cache) ─────────────────
    key = patient_id or f"unk_{random.randint(0, 999999)}"
    pdata = get_patient_data(key, diag_cancer, is_meta)

    # ── Date de sortie (date_discharge) ───────────────────
    # On la calcule à partir de date_sej + durée selon type_sejour
    d_entree = parse_date(date_sej)
    if d_entree:
        mu, sigma = DUREE_PAR_TYPE.get(type_sejour, (1, 1))
        if mu == 0:
            d_sortie = d_entree   # séjour ambulatoire = même jour
        else:
            duree = max(1, int(abs(random.gauss(mu, sigma))))
            d_sortie = d_entree + timedelta(days=duree)
        date_discharge = format_date(d_sortie)
    else:
        date_discharge = ""

    # ── Protocole et voie selon type de séjour ────────────
    protos      = PROTOCOLES_PAR_TYPE.get(type_sejour, PROTOCOLES_PAR_TYPE["?"])
    protocole   = random.choice(protos)
    voies       = VOIES_PAR_TYPE.get(type_sejour, [("Non applicable", 1.0)])
    voie_admin  = choix_pondere(voies)

    # ── Nombre de cures ───────────────────────────────────
    if type_sejour in ("S", "R"):
        nb_cures = str(random.randint(1, 12))
    else:
        nb_cures = "1"

    # ── Assembler la ligne de sortie ──────────────────────
    out = {
        "id":                  patient_id,
        "sexe":                sexe,
        "date_birth":          date_birth,
        "type_sejour":         type_sejour,
        "date_cre_sej":        date_cre_sej,
        "date_sej":            date_sej,
        "date_discharge":      date_discharge,
        "date_last_vis":       date_last,        # renommé pour global.R
        "date_death":          date_death,
        "nb_doc":              nb_doc,
        "type_pec":            type_pec,         # 1/2/3/4 → global.R mappe
        "diag_cancer":         diag_cancer,      # CIM-10 brut → global.R mappe
        "diag_associe":        diag_associe,
        "diag_meta":           diag_meta,        # "meta" / "" → global.R mappe
        "diag_antecedents":    diag_antec,       # Z85 / "" → global.R utilisera
        # Fictives
        "stade_tnm":           pdata["stade_tnm"],
        "score_oms":           pdata["score_oms"],
        "voie_administration": voie_admin,
        "protocole":           protocole,
        "nb_cures":            nb_cures,
        "imc":                 pdata["imc"],
        "tabac_paquet_an":     pdata["tabac_paquet_an"],
        "code_postal":         pdata["code_postal"],
        "ville":               pdata["ville"],
        "departement":         pdata["departement"],
        "niveau_education":    pdata["niveau_education"],
        "situation_familiale": pdata["situation_familiale"],
    }
    return out


# ──────────────────────────────────────────────
# PROGRAMME PRINCIPAL
# ──────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Enrichit le CSV CEM réel avec des colonnes fictives pour le dashboard Shiny."
    )
    parser.add_argument("--input",    "-i", default="patients_data_real.csv",
                        help="Fichier CSV d'entrée (ton vrai fichier CEM)")
    parser.add_argument("--output",   "-o", default="patients_data.csv",
                        help="Fichier CSV de sortie (utilisé par le dashboard)")
    parser.add_argument("--sep",      "-s", default=",",
                        help="Séparateur CSV : ',' (défaut) ou ';' pour Excel français")
    parser.add_argument("--encoding", "-e", default="utf-8-sig",
                        help="Encodage du fichier d'entrée (défaut: utf-8-sig pour Excel)")
    args = parser.parse_args()

    if not os.path.exists(args.input):
        print(f"\n❌ ERREUR : Fichier introuvable → '{args.input}'")
        print(f"   Assurez-vous que le fichier est dans : {os.path.abspath('.')}")
        print(f"   Ou précisez le chemin complet avec --input /chemin/vers/fichier.csv")
        sys.exit(1)

    # Taille du fichier pour estimation
    size_mb = os.path.getsize(args.input) / 1_048_576
    print(f"\n📂 Fichier d'entrée : {args.input} ({size_mb:.1f} MB)")
    print(f"📄 Fichier de sortie : {args.output}")
    print(f"🔤 Encodage : {args.encoding} | Séparateur : '{args.sep}'\n")

    n_lignes  = 0
    n_erreurs = 0

    with open(args.input,  "r", encoding=args.encoding, errors="replace", newline="") as f_in, \
         open(args.output, "w", encoding="utf-8",        newline="")                   as f_out:

        reader  = csv.DictReader(f_in, delimiter=args.sep)
        headers = reader.fieldnames

        if not headers:
            print("❌ ERREUR : Le fichier semble vide ou le séparateur est incorrect.")
            print("   Essayez --sep ';' si vos colonnes sont séparées par des points-virgules.")
            sys.exit(1)

        print(f"Colonnes détectées ({len(headers)}) :")
        for h in headers:
            print(f"  · {h}")

        # Vérifier les colonnes critiques
        required_real = ["id", "sexe", "date_birth", "type_sejour", "date_sej"]
        manquantes    = [c for c in required_real if c not in headers
                         and not any(h.lower() == c.lower() for h in headers)]
        if manquantes:
            print(f"\n⚠️  Colonnes CRITIQUES manquantes : {manquantes}")
            print("   Le script va continuer mais certaines données seront vides.")

        print(f"\n⏳ Traitement en cours...")

        writer = csv.DictWriter(f_out, fieldnames=OUTPUT_COLUMNS, extrasaction="ignore")
        writer.writeheader()

        for row in reader:
            try:
                enriched = enrich_row(row, headers)
                writer.writerow(enriched)
                n_lignes += 1
            except Exception as e:
                n_erreurs += 1
                if n_erreurs <= 5:
                    print(f"  ⚠️  Ligne {n_lignes+1} ignorée : {e}")
                continue

            if n_lignes % 50_000 == 0:
                print(f"  ✓ {n_lignes:,} lignes traitées "
                      f"({len(_patient_cache):,} patients en cache) ...")

    # ── Résumé final ──────────────────────────────────────
    print(f"\n✅ Enrichissement terminé !")
    print(f"   Lignes traitées   : {n_lignes:,}")
    print(f"   Patients uniques  : {len(_patient_cache):,}")
    print(f"   Erreurs ignorées  : {n_erreurs}")
    print(f"   Colonnes en sortie: {len(OUTPUT_COLUMNS)}")
    print(f"   Fichier produit   : {os.path.abspath(args.output)}")
    print(f"\n👉 Relancez le serveur Shiny pour exploiter les nouvelles données.")


if __name__ == "__main__":
    main()
