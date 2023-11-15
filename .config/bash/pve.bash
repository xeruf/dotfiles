
[[ $- == *i* ]] && zfs list -d 0

alias cluster='scr corosync && scr pvesr'
qcow() { qemu-img convert $1 -O qcow2 ${2:-$1}.qcow2; }
extrac() {
	for var; do
		arg=${var%.lzo}
		out=$(basename ${arg%.vma})
		lzop -x $arg.lzo
		vma extract $arg $out &&
		( cd $out && find *.raw -exec qemu-img convert {} -O qcow2 $arg-{}.qcow2 \; )
	done
}
