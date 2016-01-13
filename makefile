godel_upload:
	rsync -avz --exclude .git --exclude .ipynb_checkpoints -e ssh . godel:~/ipynb/gypsy_moth
