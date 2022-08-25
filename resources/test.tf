job "build" {
  datacenters = [
    "ap-southeast-1", "ap-southeast-2"
  ]
  update {
    stagger = "30s"
    max-parallel = 1
    immediately = true
  }
  group 'load-balancers' {
    count = 1
    ratio = 12.34
    restart {
      attempts = 10
    }
  }
