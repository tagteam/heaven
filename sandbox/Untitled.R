charlson.codes <- list(
  myocardial.infarction=c('410','DI21','DI22','DI25'),
  heart.failure=c('42709','42710','42711','42719','42899','78249','I099','DI110','DI130','DI132','DI255','DI420','DP290','DI43','DI50','DP29',paste0('DI42',5:9)),
  peripheral.vascular.disease=c('4439','441','445','DI70','DI71','DI731','DI738','DI739','DI771','DI790','DI792','DK551','DK558','DK559','DZ958','DZ959'),
  cerebrovascular.disease=c(paste0('43',0:8),paste0('I6',0:9),'DG45','DG46','DH340'),
  dementia=c('290','29309',paste0('DF0',0:3),'DG30','DF051','DG311'),
  chronic.pulmonary.disease=c(paste0('49',0:3),paste0('51',5:8),paste0('DJ4',0:7),paste0('DJ6',0:7),'DI278','DI279','DJ684','DJ701','DJ703'),
  rheumatic.disease=c('712','715','716','734','DM32','DM33','DM34','DM05','DM06','DM351','DM353','DM360','DM315'),
  peptic.ulcer.disease=c(paste0('4560',0:9),'DK25','DK26','DK27','DK28'),
  mild.liver.disease=c('571','573.01','573.04','DK713','DK714','DK715', 'DK762','DK763','DK764','DK73','DK74','DB18','DK709','DK717','DK760','DK768','DK769','DZ944'),
  severe.liver.disease=c(paste0('4560',1:9),'572','57300','DI850','DI859','DI864','DI982','DK704','DK711','DK721','DK729','DK765','DK766','DK767'),
  diabetes.without.complications=c('24900','24906','24907','24909','25000','25006','25007','25009','DE100','DE101','DE106','DE108','DE109','DE110','DE111','DE116','DE118','DE119',
                                   'DE120','DE121','DE126','DE128','DE129','DE130','DE131','DE136','DE138','DE139','DE140','DE141','DE146','DE148','DE149'),
  diabetes.with.complications=c(paste0('2490',1:5),'24908',paste0('2500',1:5),'25008',paste0('DE10',2:5),paste0('DE11',2:5),paste0('DE12',2:5),paste0('DE13',2:5),paste0('DE14',2:5),
                                'DE107','DE117','DE127','DE137','DE147'),
  hemiplegia.paraplegia=c('344',paste0('DG83',0:4),'DG81','DG82','DG041','DG114','DG801','DG802','DG839'),
  renal.disease=c('403','404',paste0('58',0:3),'584','59009','59319',paste0('7531',0:9),'792',paste0('DN03',2:7),paste0('DN05',2:7),'DZ490','DZ491','DZ492','DN18','DN19','DI120','DI131','DN250','DZ940','DZ992'),
  any.malignancy=c(paste0('1',40:72),paste0('1':74:94),paste0('20',0:7),'27559',paste0('DC0',0:39),paste0('DC4',2:6),'DC48',paste0('DC',50:75),
                   paste0('DC9',1:5),paste0('DC8',1:5),'DC88','DC90','DC96'),
  metastatic.solid.tumor=c(paste0('19',5:9),paste0('DC',76:80)),
  AIDS.HIV=c('07983','DB20','DB21','DB22','DB24')
)