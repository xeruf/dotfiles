#!/usr/bin/python3
import csv, sys, re, os, hashlib

INFILE  = os.environ.get("CSV", "clients.csv")
OUTFILE = os.environ.get("VCF", "ninjaclients.vcf")

def esc(v):
    return (v or "").replace('\\','\\\\').replace('\r','').replace('\n','\\n').replace(';','\\;').replace(',','\\,')

def fold(line):
    out=[]; 
    while len(line)>75:
        out.append(line[:75]); line=" "+line[75:]
    out.append(line); 
    return "\r\n".join(out)

def get(row, *names):
    for n in names:
        if n in row and str(row[n]).strip():
            return str(row[n]).strip()
    return ""

def split_phones(val):
    if not val: return []
    # split on common separators
    parts = re.split(r'[;,/|]|(?:\s{2,})', val)
    # also break if single string contains two numbers separated by single space with + or 0
    flat=[]
    for p in parts:
        if re.search(r'\+\d+\s+\+\d+', p) or re.search(r'0\d+\s+0\d+', p):
            flat.extend(p.split())
        else:
            flat.append(p)
    return [p.strip() for p in flat if p.strip()]

def norm_de_phone(p):
    # keep digits and leading '+'
    p = re.sub(r'[^\d+]+', '', p)
    if not p: return ""
    if p.startswith('00'):
        p = '+' + p[2:]
    elif p.startswith('+'):
        pass
    elif p.startswith('0'):
        p = '+49' + p[1:]
    else:
        # looks like bare national without 0 – assume DE
        p = '+49' + p
    return p

def uniq(seq):
    seen=set(); out=[]
    for x in seq:
        if x and x not in seen:
            seen.add(x); out.append(x)
    return out

with open(INFILE, 'r', encoding='utf-8-sig', newline='') as f:
    # sample=f.read(4096); f.seek(0)
    # try:
    #     dialect=csv.Sniffer().sniff(sample, delimiters=',;\t|')
    # except csv.Error:
    #     dialect=csv.get_dialect('excel')
    # reader=csv.DictReader(f, dialect=dialect)
    reader=csv.DictReader(f, delimiter=',', quotechar='"', doublequote=True, escapechar=None)

    cards=[]
    for i,row in enumerate(reader, start=1):
        # Core fields (German headers)
        nummer   = get(row, "Nummer")
        vorname  = get(row, "Vorname")
        nachname = get(row, "Nachname")
        name     = get(row, "Name")
        org      = get(row, "Rechnungsname") or get(row, "Name")
        email    = get(row, "E-Mail")
        web      = get(row, "Webseite")

        # Phones
        kunden_tel = split_phones(get(row, "Kunden Telefon"))
        kontakt_tel= split_phones(get(row, "Telefonnummer des Kontakts"))
        kunden_tel = uniq([norm_de_phone(x) for x in kunden_tel])
        kontakt_tel= uniq([norm_de_phone(x) for x in kontakt_tel])

        # Billing address
        street    = get(row, "Straße")
        add2      = get(row, "Adresszusatz")
        city      = get(row, "Stadt")
        state     = get(row, "Bundesland")
        zipc      = get(row, "Postleitzahl")
        country   = get(row, "Land")

        # Shipping address
        s_street  = get(row, "Strasse Versandanschrift")
        s_add2    = get(row, "Versand Adresszusatz")
        s_city    = get(row, "Stadt Versandanschrift")
        s_state   = get(row, "Versand Bundesland")
        s_zip     = get(row, "Postleitzahl Versandanschrift")
        s_country = get(row, "Lieferungsland")

        # Notes
        public_n  = get(row, "Öffentliche Notizen")
        private_n = get(row, "Interne Notizen")
        note_parts=[]
        if nummer:
            note_parts.append(f"Kundennummer: {nummer}")
        if public_n:
            note_parts.append(public_n)
        if private_n:
            note_parts.append(private_n)
        notes = "\\n---\\n".join([p for p in note_parts if p])

        # FN / N
        fn = (vorname + " " + nachname).strip() if (vorname or nachname) else (name or org or f"Client {i}")
        n_last  = nachname or ""
        n_first = vorname or ""

        # UID
        uid_src = "|".join([nummer or "", fn, email or "", org or ""]).encode("utf-8")
        uid = "in-" + hashlib.sha1(uid_src).hexdigest() + "@invoiceninja"

        lines=[]
        lines.append("BEGIN:VCARD")
        lines.append("VERSION:3.0")
        lines.append("PRODID:-//IN CSV→vCard (DE)//helper//EN")
        lines.append(fold("UID:"+uid))
        lines.append(fold("FN:"+esc(fn)))
        lines.append(fold("N:"+esc(n_last)+";"+esc(n_first)+";;;"))
        if org:  lines.append(fold("ORG:"+esc(org)))
        if email:lines.append(fold("EMAIL;TYPE=INTERNET,WORK:"+esc(email)))
        if web:  lines.append(fold("URL:"+esc(web)))

        # TELs
        for p in kunden_tel:
            lines.append(fold("TEL;TYPE=WORK,VOICE:"+esc(p)))
        for p in kontakt_tel:
            # avoid duplicates across both sets
            if p not in kunden_tel:
                lines.append(fold("TEL;TYPE=CELL,VOICE:"+esc(p)))

        # ADR billing (WORK)
        if any([street, add2, city, state, zipc, country]):
            street_join = "\\n".join([x for x in [street, add2] if x])
            adr = f"ADR;TYPE=WORK:;;{esc(street_join)};{esc(city)};{esc(state)};{esc(zipc)};{esc(country)}"
            lines.append(fold(adr))

        # ADR shipping (HOME) if present and not identical to billing
        if any([s_street, s_add2, s_city, s_state, s_zip, s_country]):
            s_street_join = "\\n".join([x for x in [s_street, s_add2] if x])
            adr2 = f"ADR;TYPE=HOME:;;{esc(s_street_join)};{esc(s_city)};{esc(s_state)};{esc(s_zip)};{esc(s_country)}"
            lines.append(fold(adr2))

        if notes:
            lines.append(fold("NOTE:"+esc(notes)))

        lines.append("END:VCARD")
        cards.append("\r\n".join(lines))

with open(OUTFILE, 'w', encoding='utf-8', newline='') as out:
    out.write("\r\n".join(cards)+("\r\n" if cards else ""))

print(f"Wrote {OUTFILE} with {len(cards)} contacts.")
