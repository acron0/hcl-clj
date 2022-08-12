(ns hcl-clj.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hcl-clj.core :as hcl-clj]))

(deftest basic-core-test
  (testing "Parsing the test file matches expected outcome - basic"
    (let [expected-outcome {:job
                            {:build
                             {:datacenters ["ap-southeast-1" "ap-southeast-2"]
                              :update {:stagger "30s"
                                       :max-parallel 1
                                       :immediately true}
                              :group {:load-balancers
                                      {:ratio 12.34
                                       :count 1
                                       :restart {:attempts 10}}}}}}]
      (is (= expected-outcome (hcl-clj/parse (slurp (io/resource "test.tf"))))))))

(deftest advanced-core-test
  (testing "Parsing the test file matches expected outcome - advanced"
    (let [expected-outcome {:variable {:cloudPassword {}},
                            :provider
                            {:vsphere
                             {:user "administrator@vmware.bertramlabs.com",
                              :password "${var.cloudPassword}",
                              :vsphere_server "10.30.21.180",
                              :version "~> 1.3.0",
                              :allow_unverified_ssl true}},
                            :data
                            {:vsphere_resource_pool
                             {:pool
                              {:name "labs-den-qa-cluster/Resources",
                               :datacenter_id "${data.vsphere_datacenter.dc.id}"}},
                             :vsphere_datastore
                             {:datastore
                              {:name "labs-qa-qnap-240",
                               :datacenter_id "${data.vsphere_datacenter.dc.id}"}},
                             :vsphere_network
                             {:network
                              {:name "VM Network",
                               :datacenter_id "${data.vsphere_datacenter.dc.id}"}},
                             :vsphere_virtual_machine
                             {:template
                              {:name "Morpheus Ubuntu 16.04.3 v1",
                               :datacenter_id "${data.vsphere_datacenter.dc.id}"}},
                             :vsphere_datacenter {:dc {:name "labs-denver"}}},
                            :resource
                            {:vsphere_virtual_machine
                             {:tm-terraform-1
                              {:disk {:label "disk0", :thin_provisioned true, :size 20},
                               :resource_pool_id "${data.vsphere_resource_pool.pool.id}",
                               :datastore_id "${data.vsphere_datastore.datastore.id}",
                               :name "tm-terraform-1",
                               :guest_id "ubuntu64Guest",
                               :memory 1024,
                               :network_interface
                               {:network_id "${data.vsphere_network.network.id}"},
                               :clone
                               {:template_uuid "${data.vsphere_virtual_machine.template.id}"},
                               :connection
                               {:type "ssh", :user "cloud-user", :password "m0rp#3us!"},
                               :num_cpus 2},
                              :tm-terraform-2
                              {:disk {:label "disk0", :thin_provisioned true, :size 20},
                               :resource_pool_id "${data.vsphere_resource_pool.pool.id}",
                               :datastore_id "${data.vsphere_datastore.datastore.id}",
                               :name "tm-terraform-2",
                               :guest_id "ubuntu64Guest",
                               :memory 512,
                               :network_interface
                               {:network_id "${data.vsphere_network.network.id}"},
                               :clone
                               {:template_uuid "${data.vsphere_virtual_machine.template.id}"},
                               :connection
                               {:type "ssh", :user "cloud-user", :password "m0rp#3us!"},
                               :num_cpus 1}}}}]
      (is (= expected-outcome (hcl-clj/parse (slurp (io/resource "vmware.tf"))))))))

(comment
  (require '[hcl-clj.lexer :as hcl-lexer])
  (require '[hcl-clj.ast :as hcl-ast])
  (def foo (slurp (io/resource "test.tf")))
  (def tokens (hcl-lexer/str->tokens foo))
  (def ast (hcl-ast/tokens->ast tokens))
  (hcl-clj/parse (slurp (io/resource "test.tf"))))
