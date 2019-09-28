### Charlcon.codes.R --- 
#----------------------------------------------------------------------
## Author: Christian Torp-Pedersen
## Created: 27 september 2019
## Version: 1
## Last-Updated: 
#----------------------------------------------------------------------
### Code:
#' @export 
charlson.codes <- list(
 myocardial.infarction=c('410','I21','I22','I25'),
 heart.failure=c('42709','42710','42711','42719','42899','78249','I099','I110','I130','I132','I255','I420','P290','I43','I50','P29',paste0('I42',5:9)),
 peripheral.vascular.disease=c('4439','441','I70','I71','I731','I738','I739','I771','I790','I792','K551','K558','K559','Z958','Z959'),
 cerebrovascular.disease=c(paste0('43',0:8),paste0('I6',0:9),'G45','G46','H340'),
 dementia=c('290',paste0('F0',0:3),'G30','F051','G311'),
 chronic.pulmonary.disease=c(paste0('49',0:3),paste0('J4',0:7),paste0('J6',0:7),'I278','I279','J684','J701','J703'), 
 rheumatic.disease=c('710','711','712','715','M32','M33','M34','M05','M06','M351','M353','M360','M315'),
 peptic.ulcer.disease=c('531','532','533','534','K25','K26','K27','K28'),
 mild.liver.disease=c('571','573.01','573.04','K713','K714','K715', 'K762','K763','K764','K73','K74','B18','K709','K717','K760','K768','K769','Z944'),
 severe.liver.disease=c(paste0('4560',1:9),'572','57300','I850','I859','I864','I982','K704','K711','K721','K729','K765','K766','K767'),
 diabetes.without.complications=c('24900','24906','24907','24909','25000','25006','25007','25009','E100','E101','E106','E108','E109','E110','E111','E116','E118','E119',
                          'E120','E121','E126','E128','E129','E130','E131','E136','E138','E139','E140','E141','E146','E148','E149'),
 diabetes.with.complications=c(paste0('2490',1:5),'24908',paste0('2500',1:5),'25008',paste0('E10',2:5),paste0('E11',2:5),paste0('E12',2:5),paste0('E13',2:5),paste0('E14',2:5),
                          'E107','E117','E127','E137','E147'),
 hemiplegia.paraplegia=c('344',paste0('G83',0:4),'G81','G82','G041','G114','G801','G802','G839'),
 renal.disease=c('403','404',paste0('58',0:3),'584','59009','59319',paste0('7531',0:9),'792',paste0('N03',2:7),paste0('N05',2:7),'Z490','Z491','Z492','N18','N19','I120','I131','N250','Z940','Z992'),
 any.malignancy=c(paste0('1',40:69),paste0(172:191),paste0('192',0:48),'193','194',paste0('C0',0:39),paste0('C4',2:6),'C48',paste0('C',50:75),
                           paste0('C9',1:5),paste0('C8',1:5),'C88','C90','C96'),
 metastatic.solid.tumor=c(paste0('19',5:9),paste0('C',76:80)),
 AIDS.HIV=c('07983','B20','B21','B22','B24')
 )

