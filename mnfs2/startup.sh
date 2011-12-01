gnome-terminal -x python ~/Projects/mnfs2/authserver.py &
sleep 1
gnome-terminal -x python ~/Projects/mnfs2/dirserver.py &
sleep 1
gnome-terminal -x python ~/Projects/mnfs2/fileserver.py fsroot1 &
sleep 1
gnome-terminal -x python ~/Projects/mnfs2/fileserver.py fsroot2 &
