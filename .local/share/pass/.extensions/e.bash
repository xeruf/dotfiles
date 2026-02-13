# Edit relative to pwd
pass edit $(case "$PWD" in
	("$PASSWORD_STORE_DIR"*) echo "${PWD#$PASSWORD_STORE_DIR}/$1";;
	(*) echo $1;;
esac)
