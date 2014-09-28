/*
一个分页的通用方法，只需要输入
offset 当前页元素的起始偏移量
sum    所有元素个数
urlPattern 点击页码时跳转的url，需要把offset信息放在querystring最后
比如，searchxxxx?keyword=hadoop&type=pdf&offset=

通过itemNum，制定每页显示的元素个数，默认为100个
通过pageItemNum，制定页码列表个数，默认为10个

返回数据为一个json格式的map：
pageHtml包含分页列表，用td包装
currentPage为当前页码
totalPage为总页码

样式
------------------------------------
上一页 2 3 4 5 6 7 8 9 10 下一页
       当前#页/共#页
------------------------------------

调用方法getPagnation(0, 1000, "searchxxxx?keyword=hadoop&type=pdf&offset=")
*/
function getPagnation(offset, sum, urlPattern) {
  var itemNum = 100;    //每页显示个数
  var pageItemNum = 10; //页码列表个数

  var totalPage = parseInt(sum/itemNum);
  if(sum % itemNum !== 0) {
      totalPage = totalPage+1;
  }
  var currentPage = parseInt(offset/itemNum)+1;

  var pageHtml = ["<tr>"];
  var startpage = currentpage;
  if(totalpage  <= pageitemnum) {
      startpage = 1;
  } else {
      var balancepage = totalpage - pageitemnum + 1;
      if (balancepage < startpage) {
          startpage = balancepage;
      }
  }
  var td = "<td align=center>";
  for(var i=startpage,limit=0;i<=totalpage && limit < pageitemnum; i++, limit++) {
      if(i !== 1 && i === startpage) {
          pagehtml.push(td + pagelink(currentpage-1, urlpattern, "上一页") + "</td>");    
      }
      if (i === currentpage) {
          pagehtml.push(td + i + "</td>");
      } else {
          pagehtml.push(td+ pagelink(i, urlpattern, i) + "</td>");    
      }
      
  }
  if (i < totalpage) {
      pagehtml.push(td + pagelink(currentpage+1, urlpattern, "下一页") + "</td>");    
  };
  pagehtml.push("</tr>");
  return {
   pageHtml: pageHtml.join(""),
   currentPage: currentPage,
   totalPage: totalPage 
  };
}
function pageLink(pageNo, urlPattern ,linkName) {
    return "<a href=" + urlPattern + (pageNo-1)*100 + ">" + linkName + "</a>";
}
