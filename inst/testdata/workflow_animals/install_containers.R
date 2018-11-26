processx::run("docker", c("build", "-t", "certigo/plot_animal_cuteness", pkgload:::shim_system.file('testdata/workflow_animals/containers/plot_animal_cuteness/', package = 'certigo')), echo = F)
