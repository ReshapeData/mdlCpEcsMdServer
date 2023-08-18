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
#' materialEcsServer()
materialEcsServer <- function(input,output,session,dms_token) {
  var_txt_materialSourceSync_manually <- tsui::var_text('txt_materialSourceSync_manually')
  var_materialERP <- tsui::var_ListChoose1("materialERP")
  var_date_materialSourceSync_auto=tsui::var_date('date_materialSourceSync_auto')
  #日志查询
  shiny::observeEvent(input$btn_materialSourceSync_log,{
    FNumber <- var_txt_materialSourceSync_manually()
    if(FNumber==""){
      tsui::pop_notice("请输入物料代码")


    }else{
      token= dms_token


      data=mdlCpEcsMdr::materialLog_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_materialSourceSync_dataView',data = data )

    }

  })
  #更新同步状态
  shiny::observeEvent(input$btn_materialSourceSync_update,{
    FNumber <- var_txt_materialSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入物料代码")

    }else{
      token= dms_token

      FName <- var_materialERP()
      mdlCpEcsMdr::materialStatus_upload(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步状态更新成功")

    }

  })

  #按单同步

  shiny::observeEvent(input$btn_materialSourceSync_manually,{
    FNumber <- var_txt_materialSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入物料代码")

    }else{
      token= dms_token

      FName <- var_materialERP()
      mdlCpEcsMdr::materialByFNumber_sync(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$btn_materialByFNumber_query,{
    FNumber <- var_txt_materialSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入物料代码")

    }else{
      token= dms_token

      data=mdlCpEcsMdr::materialByFNumber_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_materialSourceSync_dataView',data = data )

    }

  })
  #手动同步
  shiny::observeEvent(input$btn_materialSourceSync_auto,{

    token= dms_token
    FDate=var_date_materialSourceSync_auto()
    FName <- var_materialERP()
    mdlCpEcsMdr::materialByDate_sync(token = token,FDate =FDate , FName =FName )
    tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_materialSourceSync_query,{

    token= dms_token
    FDate=var_date_materialSourceSync_auto()

    data=mdlCpEcsMdr::materialByDate_query(token = token,FDate =FDate )
    tsui::run_dataTable2(id ='dt_materialSourceSync_dataView',data = data )



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
#' materialErpServer()
materialErpServer <- function(input,output,session,dms_token) {
  var_materialERP2 <- tsui::var_ListChoose1("materialERP2")

  var_txt_materialERP_manually=tsui::var_text('txt_materialERP_manually')

  #按单查询
  shiny::observeEvent(input$btn_materialERP_manually,{
    FNumber=var_txt_materialERP_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入物料编码")

    }else{
      token= dms_token

      FName=var_materialERP2()
      data = mdlCpEcsMdr::materialErpDataByFNumber_query(token = token,FNumber = FNumber,FName = FName)
      tsui::run_dataTable2(id = "dt_materialSourceSync_dataView",data = data)



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
#' materialServer()
materialServer <- function(input,output,session,dms_token) {
  #演示功用1
  materialEcsServer(input,output,session,dms_token)
  materialErpServer(input,output,session,dms_token)


}
