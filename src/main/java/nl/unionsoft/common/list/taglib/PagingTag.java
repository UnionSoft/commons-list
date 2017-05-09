package nl.unionsoft.common.list.taglib;

import static nl.unionsoft.common.list.taglib.TagUtils.renderLink;

import java.io.IOException;
import java.util.List;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.TagSupport;

import nl.unionsoft.common.list.model.ListResponse;
import nl.unionsoft.common.list.model.Sort;

public class PagingTag extends TagSupport {

    private static final long serialVersionUID = 6299019626137731683L;
    private ListResponse<?> listResponse;
    private String queryString;

    @Override
    public int doStartTag() throws JspException {
        if (listResponse != null) {
            try {
                final String sortsUrl = renderSortsUrl();
                final JspWriter out = pageContext.getOut();
                out.print(renderLink(renderFirstUrl() + sortsUrl, "First", queryString));
                out.print("||");
                out.print(renderLink(renderPreviousUrl() + sortsUrl, "Previous", queryString));
                out.print("||");
                out.print(renderLink(renderNextUrl() + sortsUrl, "Next", queryString));
                out.print("||");
                out.print(renderLink(renderLastUrl() + sortsUrl, "Last", queryString));
                out.print("||");
                out.print(renderLink(renderPageSizeAllUrl() + sortsUrl, "All", queryString));
                out.print("||");
                out.print(renderLink(renderPageSizeUrl(10) + sortsUrl, "10", queryString));
                out.print("||");
                out.print(renderLink(renderPageSizeUrl(20) + sortsUrl, "20", queryString));
                out.print("||");
                out.print(renderLink(renderPageSizeUrl(50) + sortsUrl, "50", queryString));
                out.print("||");
                out.print(renderLink(renderPageSizeUrl(100) + sortsUrl, "100", queryString));
            } catch(final IOException ex) {
                throw new IllegalStateException("Exception writing to JSP", ex);
            }
        }
        return SKIP_BODY;
    }

    private String renderSortsUrl() {
        final StringBuilder result = new StringBuilder();
        final List<Sort> sorts = listResponse.getSorts();
        if (sorts != null) {
            for (final Sort sort : sorts) {
                result.append("&sort=");
                result.append(sort.getStringValue());
            }
        }
        return result.toString();
    }

    private String renderFirstUrl() {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(listResponse.getMaxResults());
        result.append("&firstResult=0");
        return result.toString();
    }

    private String renderPreviousUrl() {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(listResponse.getMaxResults());
        result.append("&firstResult=");
        result.append(listResponse.getOffset() - listResponse.getMaxResults() < 0 ? 0 : listResponse.getOffset() - listResponse.getMaxResults());
        return result.toString();
    }

    private String renderNextUrl() {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(listResponse.getMaxResults());
        result.append("&firstResult=");
        long firstResult = 0;
        if (listResponse.getOffset() + listResponse.getMaxResults() >= listResponse.getTotalRecords() - listResponse.getMaxResults()) {
            firstResult = listResponse.getTotalRecords() - listResponse.getMaxResults();
        } else {
            firstResult = listResponse.getOffset() + listResponse.getMaxResults();
        }
        if (firstResult < 0) {
            firstResult = 0;
        }
        result.append(firstResult);
        return result.toString();
    }

    private String renderLastUrl() {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(listResponse.getMaxResults());
        result.append("&firstResult=");
        result.append(listResponse.getTotalRecords() - listResponse.getMaxResults() < 0 || listResponse.getMaxResults() == 0 ? 0 : listResponse.getTotalRecords()
                - listResponse.getMaxResults());
        return result.toString();
    }

    private String renderPageSizeAllUrl() {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=0&firstResult=0");
        return result.toString();
    }

    private String renderPageSizeUrl(int pageSize) {

        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(pageSize);
        result.append("&firstResult=");
        result.append(listResponse.getOffset() + pageSize <= listResponse.getTotalRecords() ? listResponse.getOffset() : listResponse.getTotalRecords() - pageSize > 0 ? listResponse
            .getTotalRecords() - pageSize : 0);
        return result.toString();

    }

    public ListResponse<?> getListResponse() {
        return listResponse;
    }

    public void setListResponse(ListResponse<?> listResponse) {
        this.listResponse = listResponse;
    }

    public String getQueryString() {
        return queryString;
    }

    public void setQueryString(String queryString) {
        this.queryString = queryString;
    }

}
