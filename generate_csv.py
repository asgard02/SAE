"""
CEM Rennes – Générateur de données patients (version enrichie)
--------------------------------------------------------------
Ce script crée un fichier CSV simulant des données oncologiques réalistes
pour le dashboard de démonstration. Les données sont FICTIVES.

Nouvelles colonnes v3 (27 colonnes au total) :
  code_postal, ville, departement, stade_tnm, score_oms,
  voie_administration, protocole, nb_cures, imc,
  tabac_paquet_an, niveau_education, situation_familiale

Pour lancer : python generate_csv.py
"""

import csv
import math
import os
import random
from datetime import datetime, date, timedelta
from dateutil.relativedelta import relativedelta

# Graine aléatoire fixe : les mêmes données à chaque exécution
random.seed(42)

# ──────────────────────────────────────────────
# CONFIGURATION
# ──────────────────────────────────────────────
# Nombre de patients à simuler.
# Chaque patient génère en moyenne ~5 séjours → 800 patients ≈ 4 000 lignes.
N_PATIENTS  = 2000
OUTPUT_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "patients_data.csv")


# ──────────────────────────────────────────────
# TABLES DE RÉFÉRENCE CLINIQUES
# ──────────────────────────────────────────────

# Types de cancer avec leurs probabilités (somme = 1.0)
CANCERS = [
    ("C34 (Poumon)",    0.18),
    ("C50 (Sein)",      0.16),
    ("C61 (Prostate)",  0.14),
    ("C18 (Côlon)",     0.12),
    ("C25 (Pancréas)",  0.07),
    ("C22 (Foie)",      0.06),
    ("C56 (Ovaire)",    0.05),
    ("C67 (Vessie)",    0.05),
    ("C43 (Mélanome)",  0.05),
    ("C91 (Leucémie)",  0.04),
    ("C81 (Hodgkin)",   0.04),
    ("C16 (Estomac)",   0.04),
]

# Comorbidités associées (maladies secondaires)
COMORBIDITES = [
    ("N18 (Insuf. Rénale)",    0.12),
    ("E11 (Diabète T2)",       0.15),
    ("I50 (Insuf. Cardiaque)", 0.10),
    ("J44 (BPCO)",             0.08),
    ("I10 (HTA)",              0.14),
    ("E66 (Obésité)",          0.07),
    ("",                       0.34),   # pas de comorbidité
]

# Antécédents médicaux du patient
ANTECEDENTS = [
    ("Tabagisme",    0.22),
    ("Alcoolisme",   0.10),
    ("Cardiopathie", 0.08),
    ("Obésité",      0.08),
    ("Diabète",      0.07),
    ("",             0.45),   # pas d'antécédent
]

# Types de prise en charge (traitements)
TYPES_PEC = [
    ("Chimiothérapie",   0.22),
    ("Radiothérapie",    0.15),
    ("Chirurgie",        0.13),
    ("Soins Palliatifs", 0.10),
    ("Suivi",            0.12),
    ("HDJ",              0.08),
    ("Immunothérapie",   0.07),
    ("Hormonothérapie",  0.06),
    ("Consultation",     0.04),
    ("Thérapie Ciblée",  0.03),
]

# Types de séjour : HC = Hospitalisation Complète, HDJ = Hôpital De Jour
TYPES_SEJOUR   = ["HC", "HDJ", "Consultation"]
POIDS_SEJOUR   = [0.45, 0.35, 0.20]

# Durée de séjour (moyenne, écart-type) en jours pour les HC, par cancer
DUREE_SEJOUR_PAR_CANCER = {
    "C34 (Poumon)":    (8,  5),
    "C50 (Sein)":      (5,  3),
    "C61 (Prostate)":  (6,  4),
    "C18 (Côlon)":     (9,  6),
    "C25 (Pancréas)":  (12, 7),
    "C22 (Foie)":      (10, 6),
    "C56 (Ovaire)":    (7,  4),
    "C67 (Vessie)":    (6,  3),
    "C43 (Mélanome)":  (4,  2),
    "C91 (Leucémie)":  (14, 8),
    "C81 (Hodgkin)":   (6,  3),
    "C16 (Estomac)":   (10, 5),
}
DUREE_DEFAUT = (7, 4)

# Probabilité de survie à 5 ans (valeurs cliniques approximatives)
SURVIE_5ANS = {
    "C34 (Poumon)":    0.20,
    "C50 (Sein)":      0.85,
    "C61 (Prostate)":  0.90,
    "C18 (Côlon)":     0.65,
    "C25 (Pancréas)":  0.10,
    "C22 (Foie)":      0.18,
    "C56 (Ovaire)":    0.50,
    "C67 (Vessie)":    0.77,
    "C43 (Mélanome)":  0.92,
    "C91 (Leucémie)":  0.60,
    "C81 (Hodgkin)":   0.87,
    "C16 (Estomac)":   0.30,
}
SURVIE_DEFAUT = 0.55


# ──────────────────────────────────────────────
# NOUVELLES TABLES : GÉOGRAPHIE BRETAGNE
# ──────────────────────────────────────────────

# Villes de Bretagne avec leurs codes postaux et département
# Format : (ville, code_postal, departement, poids)
LIEUX_BRETAGNE = [
    # Ille-et-Vilaine (35) — le plus peuplé, surtout Rennes
    ("Rennes",        "35000", "Ille-et-Vilaine (35)", 0.28),
    ("Saint-Malo",    "35400", "Ille-et-Vilaine (35)", 0.06),
    ("Fougères",      "35300", "Ille-et-Vilaine (35)", 0.04),
    ("Vitré",         "35500", "Ille-et-Vilaine (35)", 0.03),
    ("Redon",         "35600", "Ille-et-Vilaine (35)", 0.03),
    # Finistère (29)
    ("Brest",         "29200", "Finistère (29)",        0.14),
    ("Quimper",       "29000", "Finistère (29)",        0.08),
    ("Morlaix",       "29600", "Finistère (29)",        0.03),
    ("Concarneau",    "29900", "Finistère (29)",        0.02),
    # Morbihan (56)
    ("Vannes",        "56000", "Morbihan (56)",         0.09),
    ("Lorient",       "56100", "Morbihan (56)",         0.07),
    ("Pontivy",       "56300", "Morbihan (56)",         0.02),
    # Côtes-d'Armor (22)
    ("Saint-Brieuc",  "22000", "Côtes-d'Armor (22)",   0.06),
    ("Lannion",       "22300", "Côtes-d'Armor (22)",   0.02),
    ("Dinan",         "22100", "Côtes-d'Armor (22)",   0.03),
]

# Stade TNM selon l'agressivité du cancer
# Cancers agressifs → stade III/IV plus fréquent
STADES_PAR_CANCER = {
    "C34 (Poumon)":   [("I", 0.15), ("II", 0.20), ("III", 0.30), ("IV", 0.35)],
    "C25 (Pancréas)": [("I", 0.10), ("II", 0.15), ("III", 0.25), ("IV", 0.50)],
    "C22 (Foie)":     [("I", 0.12), ("II", 0.18), ("III", 0.30), ("IV", 0.40)],
    "C16 (Estomac)":  [("I", 0.15), ("II", 0.20), ("III", 0.30), ("IV", 0.35)],
    "C50 (Sein)":     [("I", 0.30), ("II", 0.35), ("III", 0.20), ("IV", 0.15)],
    "C43 (Mélanome)": [("I", 0.40), ("II", 0.30), ("III", 0.20), ("IV", 0.10)],
    "C61 (Prostate)": [("I", 0.25), ("II", 0.35), ("III", 0.25), ("IV", 0.15)],
}
STADE_DEFAUT = [("I", 0.25), ("II", 0.30), ("III", 0.25), ("IV", 0.20)]

# Protocoles thérapeutiques par type de prise en charge
PROTOCOLES_PAR_PEC = {
    "Chimiothérapie":  ["FOLFOX", "FOLFIRI", "AC-T", "BEP", "CHOP", "R-CHOP", "Gemcitabine", "Carboplatine"],
    "Radiothérapie":   ["IMRT", "SBRT", "3D-CRT", "Protonthérapie", "Curiethérapie"],
    "Chirurgie":       ["Résection", "Mastectomie", "Prostatectomie", "Lobectomie", "Colectomie"],
    "Immunothérapie":  ["Pembrolizumab", "Nivolumab", "Ipilimumab", "Atézolizumab"],
    "Hormonothérapie": ["Tamoxifène", "Létrozole", "Anastrozole", "Enzalutamide", "Leuproréline"],
    "Thérapie Ciblée": ["Trastuzumab", "Imatinib", "Sunitinib", "Sorafénib", "Vémurafénib"],
    "Soins Palliatifs":["Morphine IV", "Kétamine", "Midazolam", "Soins de confort"],
    "Suivi":           ["Surveillance", "Bilan biologique", "Scanner de contrôle"],
    "HDJ":             ["Transfusion", "Biphosphonates", "G-CSF", "Antiémétiques"],
    "Consultation":    ["Consultation initiale", "Avis spécialisé", "RCP"],
}

# Voie d'administration selon le traitement
VOIE_PAR_PEC = {
    "Chimiothérapie":  [("IV (Intraveineuse)", 0.70), ("Orale", 0.25), ("SC (Sous-cutanée)", 0.05)],
    "Radiothérapie":   [("Externe", 1.00)],
    "Chirurgie":       [("Chirurgicale", 1.00)],
    "Immunothérapie":  [("IV (Intraveineuse)", 0.85), ("SC (Sous-cutanée)", 0.15)],
    "Hormonothérapie": [("Orale", 0.65), ("IM (Intramusculaire)", 0.25), ("SC (Sous-cutanée)", 0.10)],
    "Thérapie Ciblée": [("Orale", 0.60), ("IV (Intraveineuse)", 0.40)],
    "Soins Palliatifs":[("IV (Intraveineuse)", 0.50), ("SC (Sous-cutanée)", 0.30), ("Orale", 0.20)],
    "Suivi":           [("Non applicable", 1.00)],
    "HDJ":             [("IV (Intraveineuse)", 0.80), ("Orale", 0.20)],
    "Consultation":    [("Non applicable", 1.00)],
}

# Niveau d'éducation
NIVEAUX_EDUCATION = [
    ("Primaire",      0.18),
    ("Secondaire",    0.45),
    ("Universitaire", 0.37),
]

# Situation familiale
SITUATIONS_FAMILIALES = [
    ("Célibataire",  0.22),
    ("Marié(e)",     0.48),
    ("Divorcé(e)",   0.15),
    ("Veuf/Veuve",   0.15),
]


# ──────────────────────────────────────────────
# FONCTIONS UTILITAIRES
# ──────────────────────────────────────────────

def choix_pondere(options):
    """
    Choisit un élément aléatoire parmi une liste de tuples (valeur, poids).
    Exemple : choix_pondere([("A", 0.7), ("B", 0.3)]) → "A" ou "B"
    """
    labels, poids = zip(*options)
    return random.choices(labels, weights=poids, k=1)[0]


def date_aleatoire(debut, fin):
    """
    Retourne une date aléatoire entre debut et fin (inclus).
    """
    delta = (fin - debut).days
    if delta <= 0:
        return debut
    return debut + timedelta(days=random.randint(0, delta))


def generer_imc(cancer, antecedent):
    """
    Génère un IMC réaliste selon le cancer et les antécédents.
    Les patients avec Obésité ont un IMC plus élevé.
    Les cancers agressifs peuvent causer cachexie (IMC bas).
    """
    if antecedent == "Obésité":
        return round(random.gauss(32, 4), 1)
    elif cancer in ("C25 (Pancréas)", "C22 (Foie)", "C34 (Poumon)"):
        # Cachexie fréquente → IMC plus bas
        return round(max(16, random.gauss(21, 3)), 1)
    else:
        return round(max(16, min(45, random.gauss(25, 4))), 1)


def generer_tabac_paquet_an(antecedent, cancer):
    """
    Génère la consommation tabagique en paquets-années.
    Un non-fumeur a 0. Un fumeur (antécédent Tabagisme) a 10-50.
    Les cancers liés au tabac peuvent avoir des valeurs plus élevées.
    """
    if antecedent != "Tabagisme" and cancer not in ("C34 (Poumon)", "C67 (Vessie)"):
        # Fumeur passif ou non-fumeur → parfois 0-5
        return round(random.choices([0, random.uniform(1, 5)], weights=[0.85, 0.15])[0], 1)
    else:
        # Fumeur confirmé
        return round(max(5, random.gauss(25, 12)), 1)


# ──────────────────────────────────────────────
# GÉNÉRATEUR PRINCIPAL : un patient
# ──────────────────────────────────────────────

def simuler_patient(pid, aujourd_hui=date(2024, 6, 30)):
    """
    Simule un patient complet avec tous ses séjours hospitaliers.
    Retourne une liste de lignes (une par séjour) prêtes pour le CSV.

    Arguments :
        pid         : identifiant unique du patient (ex: "p100007")
        aujourd_hui : date de référence pour couper les données futures
    """

    # ── Démographie ──────────────────────────────────────
    annee_naissance  = random.randint(1930, 1985)
    mois_naissance   = random.randint(1, 12)
    jour_naissance   = random.randint(1, 28)
    date_naissance   = date(annee_naissance, mois_naissance, jour_naissance)
    sexe             = random.choice([1, 2])   # 1 = Homme, 2 = Femme

    # Choisir le cancer (en évitant les cancers sexe-spécifiques incohérents)
    cancer = choix_pondere(CANCERS)
    if cancer == "C50 (Sein)" and sexe == 1:
        cancer = choix_pondere([c for c in CANCERS if c[0] != "C50 (Sein)"])
    if cancer == "C61 (Prostate)" and sexe == 2:
        cancer = choix_pondere([c for c in CANCERS if c[0] not in ("C61 (Prostate)", "C50 (Sein)")])

    # ── Données géographiques (fixes pour tout le patient) ──
    # LIEUX_BRETAGNE est une liste de 4-tuples, on choisit selon le poids (4e élément)
    noms_lieux  = [l[0] for l in LIEUX_BRETAGNE]
    poids_lieux = [l[3] for l in LIEUX_BRETAGNE]
    idx_lieu    = random.choices(range(len(LIEUX_BRETAGNE)), weights=poids_lieux)[0]
    ville, code_postal, departement, _ = LIEUX_BRETAGNE[idx_lieu]

    # ── Données sociales (fixes pour tout le patient) ──
    niveau_education    = choix_pondere(NIVEAUX_EDUCATION)
    situation_familiale = choix_pondere(SITUATIONS_FAMILIALES)

    # ── Données cliniques à l'entrée ──────────────────────
    antecedent   = choix_pondere(ANTECEDENTS)
    comorbidite  = choix_pondere(COMORBIDITES)
    imc          = generer_imc(cancer, antecedent)
    tabac_pa     = generer_tabac_paquet_an(antecedent, cancer)
    stade_tnm    = choix_pondere(STADES_PAR_CANCER.get(cancer, STADE_DEFAUT))

    # Score OMS/ECOG : 0 = actif, 4 = grabataire
    # Stade IV → score OMS plus élevé en moyenne
    if stade_tnm == "IV":
        score_oms = random.choices([0, 1, 2, 3, 4], weights=[0.05, 0.15, 0.30, 0.35, 0.15])[0]
    elif stade_tnm == "III":
        score_oms = random.choices([0, 1, 2, 3, 4], weights=[0.10, 0.30, 0.35, 0.20, 0.05])[0]
    else:
        score_oms = random.choices([0, 1, 2, 3, 4], weights=[0.35, 0.35, 0.20, 0.08, 0.02])[0]

    # ── Date de diagnostic ────────────────────────────────
    annee_diag_min  = annee_naissance + 20   # diagnostiqué après 20 ans minimum
    debut_diag      = date(max(annee_diag_min, 1990), 1, 1)
    fin_diag        = date(2020, 12, 31)
    date_diagnostic = date_aleatoire(debut_diag, fin_diag)

    # ── Statut métastatique ───────────────────────────────
    # Cancers agressifs → plus de chances d'être M1 (métastasé)
    prob_m1 = 0.40 if cancer in ("C34 (Poumon)", "C25 (Pancréas)", "C22 (Foie)", "C16 (Estomac)") else 0.25
    # Stade IV implique systématiquement des métastases
    if stade_tnm == "IV":
        prob_m1 = 0.90
    diag_meta = "M1 (Métastases)" if random.random() < prob_m1 else "M0 (Pas de métastase)"

    # ── Survie ────────────────────────────────────────────
    survie_base = SURVIE_5ANS.get(cancer, SURVIE_DEFAUT)
    if diag_meta == "M1 (Métastases)":
        survie_base *= 0.40   # M1 réduit significativement la survie
    decede = random.random() > survie_base

    if decede:
        mois_survie_moy = survie_base * 60 + 12
        mois_survie     = max(3, int(random.lognormvariate(math.log(mois_survie_moy), 0.7)))
        date_deces      = date_diagnostic + relativedelta(months=mois_survie)
        if date_deces > aujourd_hui:
            date_deces = aujourd_hui - timedelta(days=random.randint(10, 300))
        date_deces_str  = date_deces.strftime("%d/%m/%Y")
    else:
        date_deces      = None
        date_deces_str  = ""

    # ── Génération des séjours ────────────────────────────
    nb_sejours  = random.choices([1, 2, 3, 4, 5, 6], weights=[0.30, 0.25, 0.20, 0.12, 0.08, 0.05])[0]
    date_limite = (date_deces or aujourd_hui) - timedelta(days=1)

    sejours = []
    date_courante = date_diagnostic + timedelta(days=random.randint(10, 180))

    for _ in range(nb_sejours):
        if date_courante >= date_limite:
            break   # plus de séjours possibles après le décès ou aujourd'hui

        type_sejour = random.choices(TYPES_SEJOUR, weights=POIDS_SEJOUR)[0]
        type_pec    = choix_pondere(TYPES_PEC)

        # Durée de séjour
        if type_sejour == "HC":
            mu, sigma = DUREE_SEJOUR_PAR_CANCER.get(cancer, DUREE_DEFAUT)
            duree = max(1, int(random.gauss(mu, sigma)))
        else:
            duree = 0   # HDJ et Consultations = 0 nuit

        date_entree   = date_courante
        date_sortie   = date_entree + timedelta(days=duree)
        if date_sortie > date_limite:
            date_sortie = date_limite
            duree       = (date_sortie - date_entree).days

        # Dernière visite après la sortie
        delta_visite  = random.randint(30, 365)
        date_dern_vis = date_sortie + timedelta(days=delta_visite)
        if date_dern_vis > date_limite:
            date_dern_vis = date_limite

        # Protocole et voie d'administration selon la prise en charge
        protocoles_dispo = PROTOCOLES_PAR_PEC.get(type_pec, ["Non spécifié"])
        protocole        = random.choice(protocoles_dispo)
        voies_dispo      = VOIE_PAR_PEC.get(type_pec, [("Non applicable", 1.0)])
        voie_admin       = choix_pondere(voies_dispo)

        # Nombre de cures (surtout pour chimio/radio)
        if type_pec in ("Chimiothérapie", "Radiothérapie", "Immunothérapie"):
            nb_cures = random.choices(range(1, 13), k=1)[0]
        elif type_pec in ("Thérapie Ciblée", "Hormonothérapie"):
            nb_cures = random.randint(1, 6)
        else:
            nb_cures = 1

        sejours.append({
            # ── Colonnes originales ──────────────────────────
            "id":                  pid,
            "sexe":                sexe,
            "date_birth":          date_naissance.strftime("%d/%m/%Y"),
            "date_diagnosis":      date_diagnostic.strftime("%d/%m/%Y"),
            "type_sejour":         type_sejour,
            "date_sej":            date_entree.strftime("%d/%m/%Y"),
            "date_discharge":      date_sortie.strftime("%d/%m/%Y"),
            "date_last_vis":       date_dern_vis.strftime("%d/%m/%Y"),
            "date_death":          date_deces_str,
            "nb_doc":              random.randint(1, 8),
            "type_pec":            type_pec,
            "diag_cancer":         cancer,
            "diag_associe":        comorbidite,
            "diag_meta":           diag_meta,
            "diag_antecedents":    antecedent,
            # ── Nouvelles colonnes géographiques ────────────
            "code_postal":         code_postal,
            "ville":               ville,
            "departement":         departement,
            # ── Nouvelles colonnes cliniques ─────────────────
            "stade_tnm":           stade_tnm,
            "score_oms":           score_oms,
            "voie_administration": voie_admin,
            "protocole":           protocole,
            "nb_cures":            nb_cures,
            "imc":                 imc,
            "tabac_paquet_an":     tabac_pa,
            # ── Nouvelles colonnes sociales ──────────────────
            "niveau_education":    niveau_education,
            "situation_familiale": situation_familiale,
        })

        # Intervalle avant le prochain séjour (1 à 8 mois)
        gap = random.randint(30, 240)
        date_courante = date_sortie + timedelta(days=gap)

    return sejours


# ──────────────────────────────────────────────
# PROGRAMME PRINCIPAL
# ──────────────────────────────────────────────

if __name__ == "__main__":
    print(f"Génération de {N_PATIENTS} patients...")
    toutes_lignes = []

    for i in range(1, N_PATIENTS + 1):
        pid    = f"p{100000 + i * 7:06d}"
        lignes = simuler_patient(pid)
        toutes_lignes.extend(lignes)

    # Tri par patient puis par date d'entrée (ordre chronologique)
    toutes_lignes.sort(key=lambda x: (x["id"], datetime.strptime(x["date_sej"], "%d/%m/%Y")))

    # Liste ordonnée des colonnes du CSV (27 colonnes)
    COLONNES = [
        # Colonnes d'origine (15)
        "id", "sexe", "date_birth", "date_diagnosis", "type_sejour",
        "date_sej", "date_discharge", "date_last_vis", "date_death",
        "nb_doc", "type_pec", "diag_cancer", "diag_associe", "diag_meta",
        "diag_antecedents",
        # Nouvelles colonnes (12)
        "code_postal", "ville", "departement",
        "stade_tnm", "score_oms", "voie_administration", "protocole",
        "nb_cures", "imc", "tabac_paquet_an",
        "niveau_education", "situation_familiale",
    ]

    with open(OUTPUT_PATH, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=COLONNES)
        writer.writeheader()
        writer.writerows(toutes_lignes)

    print(f"OK: {len(toutes_lignes)} sejours generes pour {N_PATIENTS} patients")
    print(f"   27 colonnes | Fichier : {OUTPUT_PATH}")
