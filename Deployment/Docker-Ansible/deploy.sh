#!/usr/bin/env bash

export ANSIBLE_HOST_KEY_CHECKING=False      #avoid host key checking

password () {
    read answer
    while true
    do
        if [[ "${answer}" == "y" ]]; then
            echo "Type your Ansible password:"
            read -s password
            echo $password > ~/.vault_pass.txt
            ansible_playbook+="--vault-password-file ~/.vault_pass.txt "
            break
        elif [[ "${answer}" == "n" ]]; then
            echo "You need to enter your Ansible password every single time ansible-playbooks ask for one."
            sleep 1
            ansible_playbook+="--ask-vault-pass "
            break
        else
            echo "$answer is not a valid answer! Try again.. [ y/n ]"
            read answer
        fi
    done
}

stop () {
    if [[ ${1} == "1" ]]; then
        echo -e "\nStopping Exareme services..."

        ansible_playbook_stop=${ansible_playbook}"Stop-Services.yaml --skip-tags portainer"
        ${ansible_playbook_stop}
        ansible_playbook_code=$?

    elif [[ ${1} == "2" ]]; then
        echo -e "\nStopping Portainer services..."

        ansible_playbook_stop=${ansible_playbook}"Stop-Services.yaml --skip-tags exareme"
        ${ansible_playbook_stop}
        ansible_playbook_code=$?

    elif [[ ${1} == "3" ]]; then
        echo -e "\nStopping all services..."

        ansible_playbook_stop=${ansible_playbook}"Stop-Services.yaml"
        ${ansible_playbook_stop}
        ansible_playbook_code=$?

    else
        echo "$answer is not a valid answer! Try again.. [ 1-2-3 ]"
        read answer
    fi
}

init_ansible_playbook () {
#Default ansible_playbook
ansible_playbook="ansible-playbook -i hosts.ini -c paramiko -e@vault_file.yaml "

echo -e "\nAnsible-vault gives you the simplicity of storing your Ansible password in a file. \
Place the user's sudo password in this machine for looking file \"~/.vault_pass.txt\"...\""

# --vault-password-file or --ask-vault-pass
if [[ -z $(sudo find ~/.vault_pass.txt) ]]; then
    echo -e "\nNo such file \"~/.vault_pass.txt\". Do you want to create one now? [ y/n ]"
    flag=1
    password
else
    if [[ -s $(sudo find ~/.vault_pass.txt) ]]; then
        echo -e "\nFile exists and it is not empty! Moving on..."
        ansible_playbook+="--vault-password-file ~/.vault_pass.txt "
    else
        echo -e "\nFile is empty.. Do you want to store your Ansible password in a text file?[ y/n ]"
        password
    fi
fi

}

usernamePassword () {
echo -e "\n"${1}" remote_user=\"{{"${1}"_remote_user}}\"" >> hosts.ini
echo ${1}" become_user=\"{{"${1}"_become_user}}\"" >> hosts.ini
echo ${1}" ansible_become_pass=\"{{"${1}"_become_pass}}\"" >> hosts.ini
echo -e ${1}" ansible_ssh_pass=\"{{"${1}"_ssh_pass}}\"\n" >> hosts.ini
}

infoWorker () {
echo -e "\nWhat is the ansible host for target \"${1}\"? (expecting IP)"
read answer
echo -e "\n[${1}]" >> hosts.ini
echo ${1} "ansible_host="${answer} >> hosts.ini  #check if what given is an IP

echo -e "\nWhat is the hostname for target \"${1}\"?"
read answer
echo ${1} "hostname="${answer} >> hosts.ini

echo -e "\nWhat is the data_path for target \"${1}\"?"
read answer
#Check that path ends with /
if [[ "${answer: -1}"  != "/" ]]; then
        answer=${answer}"/"
fi
echo ${1} "data_path="${answer} >> hosts.ini

usernamePassword ${1}
}

infoMaster () {
    echo -e "\nWhat is the ansible host for target \"master\"? (expecting IP)"
    read answer
    echo "master ansible_host="${answer} >> hosts.ini  #check if what given is an IP
    echo -e "\nWhat is the home path for target \"master\"?"
    read answer
    #Check that path ends with /
    if [[ "${answer: -1}"  != "/" ]]; then
        answer=${answer}"/"
    fi
    echo "master home_path="${answer} >> hosts.ini

    echo -e "\nWhat is the data path for targer \"master\"?"
    read answer
    #Check that path ends with /
    if [[ "${answer: -1}"  != "/" ]]; then
        answer=${answer}"/"
    fi
    echo "master data_path="${answer} >> hosts.ini
    usernamePassword "master"
}


chmod 755 scripts/exareme.sh scripts/restart.sh scripts/deploy_all.sh scripts/add_worker.sh scripts/stop.sh scripts/hosts.sh

while true
do
    echo -e "Choose one of the below:\n"
    echo "1:Change the exareme docker image version."
    echo "2:Change the host machines' information."
    echo "3:Change the host machines' private information."
    echo "4:Deploy everything."
    echo "5:Add a specific worker in an already initialized swarm."
    echo "6:(Re)Start services."
    echo "7:Stop services."
    echo -e "8:Exit.\n"

    read answer1
    while true
    do
        if [[ "${answer1}" == "1" ]]; then
            . scripts/exareme.sh
            break
        elif [[ "${answer1}" == "2" ]]; then
            echo -e "\nYou chose to create hosts.ini file..."
            . scripts/hosts.sh
            break
        elif [[ "${answer1}" == "3" ]]; then
            :
        elif [[ "${answer1}" == "4" ]]; then
            . scripts/exareme.sh
            . scripts/hosts.sh
            echo -e "\nYou chose to deploy everything..."
            . scripts/deploy_all.sh
            break
        elif [[ "${answer1}" == "5" ]]; then
            . scripts/exareme.sh
            . scripts/hosts.sh
            echo -e "\nYou chose to add a specific worker in an already initialized swarm.."
            . scripts/add_worker.sh
            break
        elif [[ "${answer1}" == "6" ]]; then
            . scripts/exareme.sh
            . scripts/hosts.sh
            echo -e "\nYou chose to restart Services.."
            . scripts/restart.sh
            break
        elif [[ "${answer1}" == "7" ]]; then
            echo -e "\nYou chose to stop Services.."
            . scripts/hosts.sh
            . scripts/stop.sh
            break
        elif [[ "${answer1}" == "8" ]]; then
            echo -e "\nYou chose to Exit.."
            exit 0
        else
            echo "$answer1 is not a valid answer! Try again.. [ 1-2-3-4-5-6-7-8 ]"
            read answer1
        fi
    done
done