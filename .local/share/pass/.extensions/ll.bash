# ls but with levels instead of subdirectory as argument
tree $PASSWORD_STORE_DIR -C --dirsfirst -L ${1:-3} --noreport | less --quit-if-one-screen
