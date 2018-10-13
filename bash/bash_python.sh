# Python shortcuts
alias py='bpython -i'
alias pt='py.test -s --pdb'
alias py3='python3 -i'
alias coco='coconut -t 36'
alias icoco='coco --ipython console'
alias wcoco='coco -t 36 -w'
alias jpy="jupyter notebook"

function remove_pyc() {
    find . -name "*.pyc" -exec rm -rf {} \;
}

function purge_py() {
    find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
