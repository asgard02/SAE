import urllib.request
import re

req = urllib.request.Request("https://www.centre-eugene-marquis.fr/", headers={'User-Agent': 'Mozilla/5.0'})
try:
    html = urllib.request.urlopen(req).read().decode('utf-8')
    matches = re.findall(r'<img[^>]+src="([^"]+)"', html)
    for m in matches:
        if 'logo' in m.lower() or 'svg' in m.lower():
            print("IMG:", m)
    
    css_files = re.findall(r'<link[^>]+href="([^"]+\.css)[^"]*"', html)
    for c in css_files[:2]:
        print("CSS:", c)
except Exception as e:
    print("Error:", e)
