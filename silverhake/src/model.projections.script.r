# source('model.projections.r')

aout = dget(file.path(project.datadirectory('silverhake'),'data','Silver Hake Model','R','base model for projections.txt'))

a1 = model.projections(bugs.model = 'sp3oneI', model.out = aout,C.p=c(8,10),final.year.index=23)
