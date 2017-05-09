package nl.unionsoft.common.list.taglib;

import static nl.unionsoft.common.list.taglib.TagUtils.renderLink;

import java.io.IOException;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.TagSupport;

import nl.unionsoft.common.list.model.ListResponse;

public class SortTag extends TagSupport {

    private static final long serialVersionUID = -1500011853895173400L;
    private ListResponse<?> listResponse;
    private String property;
    private String queryString;

    @Override
    public int doStartTag() throws JspException {
        if (listResponse != null) {
            try {
                final JspWriter out = pageContext.getOut();
                out.print(renderLink(renderSortUrl("ASC"), "&#x2191", queryString));
                out.print(renderLink(renderSortUrl("DESC"), "&#x2193", queryString));
            } catch(final IOException ex) {
                throw new IllegalStateException("Exception writing to JSP", ex);
            }
        }
        return SKIP_BODY;
    }

    private String renderSortUrl(String direction) {
        final StringBuilder result = new StringBuilder();
        result.append("?maxResults=");
        result.append(listResponse.getMaxResults());
        result.append("&firstResult=");
        result.append(listResponse.getOffset());
        result.append("&sort=");
        result.append(property);
        result.append('|');
        result.append(direction);
        return result.toString();
    }

    public ListResponse<?> getListResponse() {
        return listResponse;
    }

    public void setListResponse(ListResponse<?> listResponse) {
        this.listResponse = listResponse;
    }

    public String getProperty() {
        return property;
    }

    public void setProperty(String property) {
        this.property = property;
    }

    public String getQueryString() {
        return queryString;
    }

    public void setQueryString(String queryString) {
        this.queryString = queryString;
    }

}
