export DESKTOP_SESSION=plasma
export QT_IM_MODULE=plasmaim
test $(cat /etc/hostname) = "pavilion-arch" &&
  setxkbmap -option caps:swapescape
exec startplasma-x11
