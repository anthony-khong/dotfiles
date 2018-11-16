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

# Reconfigure SSH
gcloud compute config-ssh

# Rebuild Could Dev
# gcloud compute instances delete cloud-dev --keep-disks=boot
# gcloud compute instances create cloud_dev \
#     --disk name=DISK,boot=yes,auto-delete=no
# gcloud compute ssh new-instance
# Reauthenticate
# gcloud auth login

# Allow mosh
gcloud compute firewall-rules create default-allow-mosh --allow=udp:60001

# Setting up X11Forwarding on Mac
# - Install XQuartz + Restart
# - Add "-Y" flag to ssh
# - Enable X11Forwarding via /etc/ssh/sshd_config
# - (Optional) XQuartz -> Preferences -> Security -> Swap the checks
