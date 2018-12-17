
function gcp-external-ip() {
    gcloud compute instances describe $1 | grep "natIP"  | grep -Eo '([0-9]{1,3}\.){3}[0-9]{1,3}'
}

function create-cloud-dev() {
    gcloud compute instances create cloud-dev \
        --image-family ubuntu-1804-lts \
        --image-project gce-uefi-images \
        --metadata-from-file startup-script=$HOME/dotfiles/google_cloud/startup.sh \
        --machine-type="n1-standard-$1" \
        --boot-disk-size="$2GB"
}

function cloud-dev() {
    if [ "$1" = "mosh" ]; then
        mosh --ssh="ssh -i ~/.ssh/google_compute_engine" akhong@$(gcp-external-ip cloud-dev)
    elif [ "$1" = "ssh" ]; then
        gcloud compute ssh akhong@cloud-dev --ssh-flag="-YC"
    else
        gcloud compute instances $1 cloud-dev
    fi
}

function gce() {
    if [ "$2" = "create" ]; then
        gcloud compute instances create $1 \
            --image-family ubuntu-1804-lts \
            --image-project gce-uefi-images \
            --metadata-from-file startup-script=$HOME/dotfiles/google_cloud/startup.sh \
            --machine-type="n1-standard-$2" \
            --boot-disk-size="$3GB"

    elif [ "$2" = "mosh" ]; then
        mosh --ssh="ssh -i ~/.ssh/google_compute_engine" akhong@$(gcp-external-ip $1)

    elif [ "$2" = "ssh" ]; then
        gcloud compute ssh akhong@$1 --ssh-flag="-YC"

    else
        gcloud compute instances $2 $1

    fi
}
