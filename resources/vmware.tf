variable "cloudPassword" {
}
provider "vsphere" {
  user           = "administrator@vmware.bertramlabs.com"
  password       = "${var.cloudPassword}"
  vsphere_server = "10.30.21.180"
  version = "~> 1.3.0"
  # if you have a self-signed cert
  allow_unverified_ssl = true
}
data "vsphere_datacenter" "dc" {
  name = "labs-denver"
}
data "vsphere_datastore" "datastore" {
  name = "labs-qa-qnap-240"
  datacenter_id = "${data.vsphere_datacenter.dc.id}"
}
data "vsphere_resource_pool" "pool" {
  name = "labs-den-qa-cluster/Resources"
  datacenter_id = "${data.vsphere_datacenter.dc.id}"
}
data "vsphere_network" "network" {
  name = "VM Network"
  datacenter_id = "${data.vsphere_datacenter.dc.id}"
}
data "vsphere_virtual_machine" "template" {
  name = "Morpheus Ubuntu 16.04.3 v1"
  datacenter_id = "${data.vsphere_datacenter.dc.id}"
}
resource "vsphere_virtual_machine" "tm-terraform-1" {
  name = "tm-terraform-1"
  resource_pool_id = "${data.vsphere_resource_pool.pool.id}"
  datastore_id = "${data.vsphere_datastore.datastore.id}"
  num_cpus = 2
  memory = 1024
  guest_id = "ubuntu64Guest"
  network_interface {
    network_id = "${data.vsphere_network.network.id}"
  }
  disk {
    label = "disk0"
    thin_provisioned = true
    size  = 20
  }
  clone {
    template_uuid = "${data.vsphere_virtual_machine.template.id}"
  }
  connection {
    type = "ssh"
    user = "cloud-user"
    password = "m0rp#3us!"
  }
}
resource "vsphere_virtual_machine" "tm-terraform-2" {
  name = "tm-terraform-2"
  resource_pool_id = "${data.vsphere_resource_pool.pool.id}"
  datastore_id = "${data.vsphere_datastore.datastore.id}"
  num_cpus = 1
  memory = 512
  guest_id = "ubuntu64Guest"
  network_interface {
    network_id = "${data.vsphere_network.network.id}"
  }
  disk {
    label = "disk0"
    thin_provisioned = true
    size  = 20
  }
  clone {
    template_uuid = "${data.vsphere_virtual_machine.template.id}"
  }
  connection {
    type = "ssh"
    user = "cloud-user"
    password = "m0rp#3us!"
  }
}
