# hcl-clj

Convert HashiCorp HCL to Clojure hash map.
Written in pure Clojure, with GraalVM in mind (no Java interop).

``` hcl
job "build" {
  datacenters = [
      "ap-southeast-2"
  ]
  update {
      stagger = "30s"
      max-parallel = 1
  }
  group "load-balancers" {
      count = 1
      restart {
            attempts = 10
      }
  }

```
becomes

``` clojure
{:job
  {:build
    {:datacenters ["ap-southeast-2"]
     :update {:stagger "30s"
              :max-parallel 1.0}
     :group {:load-balancers
               {:count 1.0
                :restart {:attempts 10.0}}}}}}
```

## Installation

*Leiningen/Boot*

```
[org.clojars.acron0/hcl-clj "0.1.0"]
```

*Clojure CLI/deps.edn*

```
org.clojars.acron0/hcl-clj {:mvn/version "0.1.0"}
```


## Usage

``` clojure
(require '[hcl-clj.core :as hcl-clj])
(require '[clojure.java.io :as io])

(hcl-clj/parse (slurp (io/resource "test.tf")))
```

## License

Copyright © 2022 Antony Woods

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
