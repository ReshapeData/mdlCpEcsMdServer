#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' supplierEcsServer()
supplierEcsServer <- function(input,output,session,dms_token) {
  var_txt_supplierSourceSync_manually <- tsui::var_text('txt_supplierSourceSync_manually')
  var_supplierERP <- tsui::var_ListChoose1("supplierERP")
  var_date_supplierSourceSync_auto=tsui::var_date('date_supplierSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_supplierSourceSync_log,{
    FNumber <- var_txt_supplierSourceSync_manually()
    if(FNumber==""){
      tsui::pop_notice("请输入供应商代码")


    }else{
      token= dms_token


      data=mdlCpEcsMdr::supplierLog_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_supplierSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_supplierSourceSync_update,{
    FNumber <- var_txt_supplierSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入供应商代码")

    }else{
      token= dms_token

      FName <- var_supplierERP()
      mdlCpEcsMdr::supplierStatus_upload(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_supplierSourceSync_manually,{
    FNumber <- var_txt_supplierSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入供应商代码")

    }else{
      token= dms_token

      FName <- var_supplierERP()
      mdlCpEcsMdr::supplierByFNumber_sync(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$btn_supplierByFNumber_query,{
    FNumber <- var_txt_supplierSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入供应商代码")

    }else{
      token= dms_token

      data=mdlCpEcsMdr::supplierByFNumber_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_supplierSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_supplierSourceSync_auto,{

    token= dms_token
    FDate=var_date_supplierSourceSync_auto()
    FName <- var_supplierERP()
    mdlCpEcsMdr::supplierByDate_sync(token = token,FDate =FDate , FName =FName )
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_supplierSourceSync_query,{

    token= dms_token
    FDate=var_date_supplierSourceSync_auto()

    data=mdlCpEcsMdr::supplierByDate_query(token = token,FDate =FDate )
    tsui::run_dataTable2(id ='dt_supplierSourceSync_dataView',data = data )



  })

}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' supplierErpServer()
supplierErpServer <- function(input,output,session,dms_token) {
  var_supplierERP2 <- tsui::var_ListChoose1("supplierERP2")

  var_txt_supplierERP_manually=tsui::var_text('txt_supplierERP_manually')

  #按单查询
  shiny::observeEvent(input$btn_supplierERP_manually,{
    FNumber=var_txt_supplierERP_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入供应商编码")

    }else{
      token= dms_token

      FName=var_supplierERP2()
      data = mdlCpEcsMdr::supplierErpDataByFNumber_query(token = token,FNumber = FNumber,FName = FName)
      tsui::run_dataTable2(id = "dt_supplierSourceSync_dataView",data = data)



    }




  })


}







#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' supplierServer()
supplierServer <- function(input,output,session,dms_token) {
  #演示功用1
  supplierEcsServer(input,output,session,dms_token)
  supplierErpServer(input,output,session,dms_token)


}
