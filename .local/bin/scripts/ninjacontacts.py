import csv, sys, re, io, os, unicodedata, hashlib
from datetime import datetime

INFILE  = os.environ.get("CSV", "clients.csv")
OUTFILE = os.environ.get("VCF", "ninjaclients.vcf")

def norm_header(h):
    h = h.strip().lower()
    h = h.replace("\ufeff","")
    return re.sub(r'[^a-z0-9]+','_',h)

def esc(v):
    # vCard 3.0 escaping: backslash, comma, semicolon, newline
    return (v.replace('\\','\\\\')
             .replace('\n','\\n')
             .replace('\r','')
             .replace(';','\\;')
             .replace(',','\\,'))

def fold(line):
    # Fold at 75 chars with CRLF + space (vCard 3.0)
    out = []
    while len(line) > 75:
        out.append(line[:75])
        line = " " + line[75:]
    out.append(line)
    return "\r\n".join(out)

def pick(row, hdr, *keys):
    for k in keys:
        if k in hdr and row.get(hdr[k], "").strip():
            return row[hdr[k]].strip()
    return ""

def join_nonempty(*parts, sep=" "):
    return sep.join([p for p in parts if p])

with open(INFILE, "r", encoding="utf-8-sig", newline="") as f:
    sample = f.read(4096)
    f.seek(0)
    try:
        dialect = csv.Sniffer().sniff(sample, delimiters=",;\t|")
    except csv.Error:
        dialect = csv.get_dialect("excel")
    reader = csv.DictReader(f, dialect=dialect)
    # map headers -> normalized
    raw_headers = reader.fieldnames or []
    hdr_map = {norm_header(h): h for h in raw_headers}
    # reverse: normalized -> original
    inv = {norm_header(h): h for h in raw_headers}

    def H(*names):  # helper to return normalized header if present
        for n in names:
            n = norm_header(n)
            if n in inv: return {n: inv[n]}  # single-item dict
        return {}

    # Build a dictionary of normalized->original for quick pick()
    hdr = {norm_header(h): h for h in raw_headers}

    cards = []
    for idx, row in enumerate(reader, start=1):
        fn  = pick(row, hdr, "name", "full_name") or \
              join_nonempty(pick(row, hdr, "first_name","firstname","given_name"),
                            pick(row, hdr, "last_name","lastname","surname")) or \
              pick(row, hdr, "company","company_name","client_name") or \
              f"Client {idx}"

        n_last  = pick(row, hdr, "last_name","lastname","surname")
        n_first = pick(row, hdr, "first_name","firstname","given_name")

        org  = pick(row, hdr, "company","company_name","organization")
        email= pick(row, hdr, "email","e_mail","primary_email")
        phone= pick(row, hdr, "phone","phone_number","telephone")
        mobile=pick(row, hdr, "mobile","cell","mobile_phone")
        fax  = pick(row, hdr, "fax","fax_number")
        web  = pick(row, hdr, "website","web","url")

        addr1= pick(row, hdr, "address1","street","street_address","billing_street")
        addr2= pick(row, hdr, "address2","address_2","address_line_2")
        city = pick(row, hdr, "city","billing_city","town")
        state= pick(row, hdr, "state","province","region")
        post = pick(row, hdr, "postal_code","zip","postcode")
        ctry = pick(row, hdr, "country","country_name","billing_country")

        notes = join_nonempty(
            pick(row, hdr, "public_notes","public_note","notes","note"),
            pick(row, hdr, "private_notes","private_note"),
            sep="\\n---\\n"
        )

        # build UID from stable fields if possible
        uid_src = join_nonempty(
            pick(row, hdr, "id","client_id","public_id","number"),
            fn, email or "", phone or "", org or "", sep="|"
        ).encode("utf-8")
        uid = "in-" + hashlib.sha1(uid_src).hexdigest() + "@invoiceninja"

        lines = []
        lines.append("BEGIN:VCARD")
        lines.append("VERSION:3.0")
        lines.append("PRODID:-//InvoiceNinja CSV -> vCard//janek-helper//EN")
        lines.append(fold("UID:" + uid))
        if fn:   lines.append(fold("FN:" + esc(fn)))
        # N:last;first;middle;prefix;suffix
        lines.append(fold("N:" + esc(n_last) + ";" + esc(n_first) + ";;;"))
        if org:  lines.append(fold("ORG:" + esc(org)))
        if email:lines.append(fold("EMAIL;TYPE=INTERNET,WORK:" + esc(email)))
        if phone:lines.append(fold("TEL;TYPE=WORK,VOICE:" + esc(phone)))
        if mobile:lines.append(fold("TEL;TYPE=CELL,VOICE:" + esc(mobile)))
        if fax:  lines.append(fold("TEL;TYPE=FAX:" + esc(fax)))
        if web:  lines.append(fold("URL:" + esc(web)))

        if any([addr1, addr2, city, state, post, ctry]):
            street = join_nonempty(addr1, addr2, sep="\\n")
            # ADR;TYPE=WORK:PO;EXT;STREET;LOCALITY;REGION;POSTCODE;COUNTRY
            adr = f"ADR;TYPE=WORK:;;{esc(street)};{esc(city)};{esc(state)};{esc(post)};{esc(ctry)}"
            lines.append(fold(adr))

        if notes:
            lines.append(fold("NOTE:" + esc(notes)))

        lines.append("END:VCARD")
        cards.append("\r\n".join(lines))

with open(OUTFILE, "w", encoding="utf-8", newline="") as out:
    out.write("\r\n".join(cards) + ("\r\n" if cards else ""))

print(f"Wrote {OUTFILE} with {len(cards)} contacts.")
