#!/usr/bin/env bash
# Deduplicate NetworkManager wifi profiles by SSID, keeping the newest,
# and strip interface-name= so PCI renames don't invalidate them.
#
# Usage:
#   sudo bash nm-dedupe.sh           # dry run
#   sudo bash nm-dedupe.sh --apply   # actually change things

set -euo pipefail

DIR=/etc/NetworkManager/system-connections
APPLY=0
[[ "${1:-}" == "--apply" ]] && APPLY=1

[[ $EUID -eq 0 ]] || { echo "must run as root" >&2; exit 1; }
[[ -d $DIR ]]     || { echo "no $DIR" >&2; exit 1; }

TS=$(date +%Y%m%d-%H%M%S)
BACKUP=/root/nm-dedupe-backup-$TS

# Fields ignored when comparing duplicate profiles.
IGNORED_KEYS='^(id|uuid|interface-name|timestamp)='

get_ssid() {
    awk -F= '
        /^\[/  { section=$0; next }
        section ~ /^\[(wifi|802-11-wireless)\]/ && $1=="ssid" {
            sub(/^ssid=/,"",$0); print; exit
        }
    ' "$1"
}

# Drop ignored keys, blanks and comments; sort lines within each section so
# key reordering doesn't show as a difference.
normalise() {
    awk -v ignore="$IGNORED_KEYS" '
        function flush(   i) {
            if (n) { asort(buf); for (i=1;i<=n;i++) print buf[i]; n=0 }
        }
        /^\[/ { flush(); print; next }
        /^[[:space:]]*$/ || /^#/ { next }
        $0 ~ ignore { next }
        { buf[++n]=$0 }
        END { flush() }
    ' "$1"
}

declare -A BY_SSID=()
shopt -s nullglob
for f in "$DIR"/*.nmconnection; do
    ssid=$(get_ssid "$f")
    [[ -z $ssid ]] && continue
    mtime=$(stat -c %Y "$f")
    BY_SSID[$ssid]+="${mtime}	${f}"$'\n'
done

KEEP_PATHS=()
EDIT_PATHS=()
RM_PATHS=()
declare -A RENAME=()   # keeper-path -> new-path (strip " N" suffix)
WARN=()

# Returns 0 if the file's basename (sans .nmconnection) ends in " <digits>".
is_suffixed() {
    [[ "$(basename "$1" .nmconnection)" =~ \ [0-9]$ ]]
}

for ssid in "${!BY_SSID[@]}"; do
    mapfile -t entries < <(printf '%s' "${BY_SSID[$ssid]}" | sort -rn)

    # Pick keeper: prefer an unsuffixed file; else newest (entries[0]).
    keeper=""
    for e in "${entries[@]}"; do
        p=${e#*$'\t'}
        if ! is_suffixed "$p"; then keeper=$p; break; fi
    done
    [[ -z $keeper ]] && keeper=${entries[0]#*$'\t'}

    KEEP_PATHS+=("$keeper")
    grep -q '^interface-name=' "$keeper" && EDIT_PATHS+=("$keeper")

    # If keeper still has a " N" suffix, plan a rename to the canonical name.
    if is_suffixed "$keeper"; then
        base=$(basename "$keeper" .nmconnection)
        canonical="$DIR/${base% *}.nmconnection"
        [[ ! -e $canonical ]] && RENAME[$keeper]=$canonical
        # also fix the id= inside the file
        EDIT_PATHS+=("$keeper")  # will also trigger id strip below
    fi

    [[ ${#entries[@]} -le 1 ]] && continue

    keeper_norm=$(normalise "$keeper")
    for e in "${entries[@]}"; do
        loser=${e#*$'\t'}
        [[ "$loser" == "$keeper" ]] && continue
        RM_PATHS+=("$loser")
        loser_norm=$(normalise "$loser")
        if [[ "$keeper_norm" != "$loser_norm" ]]; then
            WARN+=("DIVERGENCE for SSID '$ssid' (beyond id/uuid/interface-name/timestamp):")
            WARN+=("  keeper: $keeper")
            WARN+=("  loser : $loser")
            while IFS= read -r line; do
                WARN+=("    $line")
            done < <(diff <(printf '%s\n' "$keeper_norm") \
                          <(printf '%s\n' "$loser_norm") || true)
        fi
    done
done

echo "== KEEP =="; printf '  %s\n' "${KEEP_PATHS[@]}"
echo "== EDIT (strip interface-name, fix id=) =="; printf '  %s\n' "${EDIT_PATHS[@]:-(none)}"
echo "== RM =="; printf '  %s\n' "${RM_PATHS[@]:-(none)}"
echo "== RENAME =="
if [[ ${#RENAME[@]} -eq 0 ]]; then echo "  (none)"
else for k in "${!RENAME[@]}"; do echo "  $k  ->  ${RENAME[$k]}"; done
fi

if [[ ${#WARN[@]} -gt 0 ]]; then
    echo
    echo "===== WARNINGS ====="
    printf '%s\n' "${WARN[@]}"
    echo "===================="
fi

if [[ $APPLY -eq 0 ]]; then
    echo
    echo "(dry run — re-run with --apply to make changes)"
    exit 0
fi

if [[ ${#WARN[@]} -gt 0 ]]; then
    read -rp "Divergences found above. Continue anyway? [y/N] " ans
    [[ "$ans" == "y" || "$ans" == "Y" ]] || { echo "aborted"; exit 1; }
fi

mkdir -p "$BACKUP"; chmod 700 "$BACKUP"

for p in "${RM_PATHS[@]}";  do cp -a "$p" "$BACKUP/"; rm -f "$p"; done

# de-dup EDIT_PATHS (keeper with suffix may have been added twice)
mapfile -t EDIT_PATHS < <(printf '%s\n' "${EDIT_PATHS[@]}" | awk '!seen[$0]++')

for p in "${EDIT_PATHS[@]}"; do
    [[ -e $p ]] || continue
    cp -a "$p" "$BACKUP/edit-$(basename "$p")"
    sed -i '/^interface-name=/d' "$p"
    # If id ends in " N", strip the suffix.
    sed -i -E 's/^(id=.*) [0-9]$/\1/' "$p"
done

for k in "${!RENAME[@]}"; do
    [[ -e $k ]] || continue
    [[ -e ${RENAME[$k]} ]] && { echo "skip rename, target exists: ${RENAME[$k]}" >&2; continue; }
    mv -n "$k" "${RENAME[$k]}"
done

nmcli connection reload
echo "done. backup at $BACKUP"
