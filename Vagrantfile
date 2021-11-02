# -*- mode: ruby -*-
def utop_autocomplete_config(machine)
  machine.vm.provision "file", source: "vagrant.d/.lambda-term-inputrc", destination: "$HOME/.lambda-term-inputrc", run: "always"
end

def ssh_key_permissions(machine)
  machine.vm.provision "shell", inline: "chmod u+w /home/vagrant/.ssh/authorized_keys", run: "always"
end

def workaround_monterey_headless_bug(config)
  if (/darwin/ =~ RUBY_PLATFORM) != nil
    config.vm.provider "virtualbox" do |v|
      v.gui = true
    end
  end
end

Vagrant.configure(2) do |config|
   config.vm.provider "virtualbox" do |vb|
     vb.memory = "2048"
   end
   
   config.vm.define "cs-linux" do |machine|
     machine.vm.box = "Neowizard/cs-linux"
     machine.ssh.insert_key = true
     machine.vm.synced_folder ".", "/home/vagrant/compiler"
     workaround_monterey_headless_bug(config)
     ssh_key_permissions(machine)
     utop_autocomplete_config machine
  end

end
