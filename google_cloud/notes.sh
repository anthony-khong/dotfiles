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

# Create a storage bucket
# gsutil mb gs://machine-learning-bucket/

# Install gcloud
curl https://sdk.cloud.google.com | bash
exec -l $SHELL
gcloud init

# Allow mosh
gcloud compute firewall-rules create default-allow-mosh --allow=udp:60001

# Hooking up Google Storage
# - Install gcloud
# - export BOTO_CONFIG=/dev/null
# - gsulit cp

# Setting up X11Forwarding on Mac
# - Install XQuartz + Restart
# - Add "-Y" flag to ssh
# - Enable X11Forwarding via /etc/ssh/sshd_config
# - (Optional) XQuartz -> Preferences -> Security -> Swap the checks

# Configuring iTerm2
# - Use FiraCode + Use Ligature
# - Link: https://github.com/tonsky/FiraCode

# Setting up mosh:
# - Install mosh both in client and server
# - Allow UDP connections: gcloud compute firewall-rules create mosh --allow udp:60000-61000
# - Manually parse the instance's external IP
