# List all available public images
gcloud compute images list

# Create Ubuntu instance for development
gcloud compute instances create $NAME \
    --image-family ubuntu-1804-lts \
    --image-project ubuntu-os-cloud \
    --metadata-from-file startup-script=startup.sh

# Create COS instance for production
gcloud compute instances create $NAME \
    --image-family cos-stable \
    --image-project gce-uefi-images

# Start/Delete/Stop/Reset an instance
gcloud compute instances $COMMAND $NAME

# SSH when default project is set
gcloud compute ssh $NAME

# SSH to Cloud Shell
gcloud alpha cloud-shell ssh
