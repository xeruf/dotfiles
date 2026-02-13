set -e
# autofill ssh passwords from mpw via SSH_ASKPASS
# requires the ssh config to list the Hostname first in any block
# to correctly detect the machine name from ip/domain
case "$1" in
("The authenticity"*) echo yes;;
(*@*\'*)
host="$SSH_HOST"
if test -z "$host"; then
   test $(echo "$1" | wc -l) -gt 1 && echo yes && exit 0
   hostname="$(echo "$1" | sed "s/.*@\(.*\)'.*/\1/")"
   user="$(echo "$1" | cut -d'@' -f1)"
   test -z "$hostname" &&
       echo "No hostname provided!" >&2 &&
       exit 1

   _ssh_config="$HOME/.ssh/config"
   host="$(cat "$_ssh_config" | grep -1 --word-regexp "Hostname $hostname" | head -1 | cut -d' ' -f2 || echo "$hostname")"
   test "$host" = "$1" ||
       echo "Identified target '$host' in '$_ssh_config' via hostname '$hostname' from input '$@'" >&2
fi
passcommand="pass show $(expr "app/mpw/local" \& "$user" = "janek" \| "business/ftt/mpw")"
echo -n "$($passcommand | head -1)" |
    mpw -u "$($passcommand | head -2 | tail -1)" -t $(test "$host" = "iridion.it" && echo long || echo basic) \
        -q -m - "$host"
;;
(*) SSH_HOST="$(echo "$1" | tr -d '0-9.')" SSH_ASKPASS=pass-ssh SSH_ASKPASS_REQUIRE=prefer TERM=xterm-256color ssh -t "$@" "echo '$(pass mpw ftt -t long "$1")' | sudo -p '' -Sv; \$SHELL"
;;
esac
