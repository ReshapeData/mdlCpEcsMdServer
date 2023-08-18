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
#' customerEcsServer()
customerEcsServer <- function(input,output,session,dms_token) {
  var_txt_customerSourceSync_manually <- tsui::var_text('txt_customerSourceSync_manually')
  var_customerERP <- tsui::var_ListChoose1("customerERP")
  var_date_customerSourceSync_auto=tsui::var_date('date_customerSourceSync_auto')
#日志查询
  shiny::observeEvent(input$btn_customerSourceSync_log,{
    FNumber <- var_txt_customerSourceSync_manually()
    if(FNumber==""){
      tsui::pop_notice("请输入客户代码")


    }else{
      token= dms_token


      data=mdlCpEcsMdr::customerLog_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_customerSourceSync_dataView',data = data )

    }

  })
#更新同步状态
  shiny::observeEvent(input$btn_customerSourceSync_update,{
    FNumber <- var_txt_customerSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入客户代码")

    }else{
      token= dms_token

      FName <- var_customerERP()
      mdlCpEcsMdr::customerStatus_upload(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步状态更新成功")

    }

  })

#按单同步

  shiny::observeEvent(input$btn_customerSourceSync_manually,{
    FNumber <- var_txt_customerSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入客户代码")

    }else{
      token= dms_token

      FName <- var_customerERP()
      mdlCpEcsMdr::customerByFNumber_sync(token = token,FNumber = FNumber,FName = FName )
      tsui::pop_notice("同步成功")

    }

  })
  #按单据编号查询

  shiny::observeEvent(input$btn_customerByFNumber_query,{
    FNumber <- var_txt_customerSourceSync_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入客户代码")

    }else{
      token= dms_token

      data=mdlCpEcsMdr::customerByFNumber_query(token = token,FNumber = FNumber)
      tsui::run_dataTable2(id ='dt_customerSourceSync_dataView',data = data )

    }

  })
#手动同步
  shiny::observeEvent(input$btn_customerSourceSync_auto,{

      token= dms_token
      FDate=var_date_customerSourceSync_auto()
      FName <- var_customerERP()
      mdlCpEcsMdr::customerByDate_sync(token = token,FDate =FDate , FName =FName )
      tsui::pop_notice("同步成功")


  })
  #源单查询

  shiny::observeEvent(input$btn_customerSourceSync_query,{

      token= dms_token
      FDate=var_date_customerSourceSync_auto()

      data=mdlCpEcsMdr::customerByDate_query(token = token,FDate =FDate )
      tsui::run_dataTable2(id ='dt_customerSourceSync_dataView',data = data )



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
#' customerErpServer()
customerErpServer <- function(input,output,session,dms_token) {
  var_customerERP2 <- tsui::var_ListChoose1("customerERP2")

  var_txt_customerERP_manually=tsui::var_text('txt_customerERP_manually')

  #按单查询
  shiny::observeEvent(input$btn_customerERP_manually,{
    FNumber=var_txt_customerERP_manually()

    if(FNumber==""){
      tsui::pop_notice("请输入客户编码")

    }else{
      token= dms_token

      FName=var_customerERP2()
      data = mdlCpEcsMdr::customerErpDataByFNumber_query(token = token,FNumber = FNumber,FName = FName)
      tsui::run_dataTable2(id = "dt_customerSourceSync_dataView",data = data)



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
#' customerServer()
customerServer <- function(input,output,session,dms_token) {
  #演示功用1
  customerEcsServer(input,output,session,dms_token)
  customerErpServer(input,output,session,dms_token)


}
