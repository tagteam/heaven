### CharlconcodesR --- 
#----------------------------------------------------------------------
## Author: Christian Torp-Pedersen
## Created: 27 september 2019
## Version: 1
## Last-Updated: 
#----------------------------------------------------------------------
### Code:
#' @export 
charlson.codes <- list(
 myocardial.infarction=c('410','DI21','DI22','DI25'),
 heart.failure=c('42709', '42710', '42711', '42719', '42899', '78249','DI099','DI110','DI130','DI132','DI255','DI425','DI426','DI427','DI429','DI428A','DP290','DI43','DI50'),
 peripheral.vascular.disease=c('440','441', '442', '443', '444', '445','DI70','DI71','DI72','DI731','DI738','DI739','DI77','DI790','DI792','DK551','DK558','DK559','DZ958','DZ959'),
 cerebrovascular.disease=c(paste0('43',0:8),paste0('DI6',0:9),'DG45','DG46','DH340'),
 dementia=c('290',paste0('DF0',0:3),'DG30','DF051','DG311'),
 chronic.pulmonary.disease=c( paste0('51',5:8),paste0('49',0:3),paste0('DJ4',0:7),paste0('DJ6',0:7),'DJ684','DI278','DI279','DJ84','DJ701','DJ703',
   'DJ920','DJ953','DJ961','DJ982','DJ983','DI278','DI279'), 
 rheumatic.disease=c('712', '716', '734', '446', '13599','DM05','DM06','DM08','DM09','DM30','DM31','DM32','DM33','DM34','DM35','DM35','DM36','D86'),
 peptic.ulcer.disease=c('53091', '53098', paste0('53',1:4),'DK25','DK26','DK27','DK28'),
 mild.liver.disease=c('571','57301','57304','DB18','DK700','DK701','DK702','DK709','DK703','DK713','DK714','DK715','DK717','DK73','DK74','DK760','DK762','DK763','DK764','DK769','DZ944'),
 severe.liver.disease=c('07000', '07002', '07004', '07006', '07008','57300', '45601','45602','45603','45604',
                        '45605','45606','45607','45608','45609','57300','DB150','DB160','DB162','DB19','DI850','DI859',
                        'DI864','DI982','DK704','DK711','DK721','DK729','DK765','DK766','DK767'), 
 diabetes.without.complications=c('24900', '24906', '24907', '24909','25000', '25006', '25007', '25009','DE100','DE101','DE106','DE108','DE109','DE110','DE111','DE119',
                          'DE120','DE121','DE129','DE130','DE131','DE139','DE140','DE141','DE149'),
 diabetes.with.complications=c(paste0('2490',1:5),'24908',paste0('2500',1:5),'25008',paste0('DE10',2:7),paste0('DE11',2:8),paste0('DE12',2:8),
      paste0('DE13',2:8),paste0('DE14',2:8)),
 hemiplegia.paraplegia=c('344',paste0('DG83',0:4),'DG81','DG82','DG041','DG114','DG801','DG802','DG839'),
 renal.disease=c('403','404',paste0('58',0:4),'59009','59319',paste0('7531',0:9),'792',paste0('DN03',2:7),paste0('DN05',2:7),'DZ490','DZ491','DZ492','DN18','DN19','DI120','DI131','DN250','DZ940','DZ992'),
 any.malignancy=c(paste0('1',40:72),paste0(174:194),paste0(200:207),'27559',paste0('DC',0:3),paste0('DC4',0:9),'DC5','DC6',paste0('DC7',0:6),
      paste0('DC8',1:8),paste0('DC9',0:7)),
 metastatic.solidtumor=c(paste0('19',5:9),paste0('DC',77:80)),
 AIDSHIV=c('07983','DB20','DB21','DB22',',DB23','DB24'),
 leukemia=c(paste0('20',4:7),paste0('DC9',1:5)),
 lymphoma=c(paste0('20',0:3),'27559',paste0('DC8',1:5),'DC88','DC90','DC96')
 )
 