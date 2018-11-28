processx::run("docker", c("build", "-t", "certigo/workflow_animals", pkgload:::shim_system.file('testdata/workflow_animals/container/', package = 'certigo')), echo = F)
