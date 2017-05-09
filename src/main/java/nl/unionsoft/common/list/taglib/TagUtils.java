package nl.unionsoft.common.list.taglib;

import org.apache.commons.lang.StringUtils;

public class TagUtils {
    public static String renderLink(String target, String name, String queryString) {
        final StringBuilder result = new StringBuilder();
        result.append("<a href=\"");
        result.append(target);
        if (StringUtils.isNotEmpty(queryString)) {
            result.append("&");
            result.append(queryString);
        }
        result.append("\">");
        result.append(name);
        result.append("</a>");
        return result.toString();
    }
}
