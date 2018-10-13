function docker_cleanup_images() {
    docker rmi $(docker images -q)
}

function docker_cleanup_containers() {
    docker rm $(docker ps -a -q)
}

function gpy() {
    DEST="/grist"
    docker run -v ~/grist:"$DEST" -it grist_image ipython -i "$DEST/$1"
}

function pyml() {
    docker run -v "$PWD":"/root/$(basename $PWD)" -it --entrypoint ipython -i ml-dev
}

function cocoml() {
    docker run -v "$PWD":"/root/$(basename $PWD)" -it --entrypoint coco ml-dev --ipython console
}

function docker_pypy() {
    docker run -v "$PWD":"/root/$(basename $PWD)" -it pypy
}

function pt_docker() {
    DIRNAME="$(basename $PWD)"
    docker run -v "$PWD":"/root/$DIRNAME" -it --entrypoint py.test ml-dev "$DIRNAME" -s --pdb
}
