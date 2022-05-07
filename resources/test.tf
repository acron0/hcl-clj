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
