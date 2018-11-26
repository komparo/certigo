processx::run("docker", c("build", "-t", "certigo/animal_cuteness", pkgload:::shim_system.file('testdata/workflow_animals/container/', package = 'certigo')), echo = F)
