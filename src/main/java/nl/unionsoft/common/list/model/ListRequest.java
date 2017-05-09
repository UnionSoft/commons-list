package nl.unionsoft.common.list.model;

import java.util.ArrayList;
import java.util.List;

public class ListRequest {

    public static final int DEFAULT_MAX_RESULTS = 100;
    public static final int ALL_RESULTS = -1;
    private List<Sort> sorts;
    private List<Restriction> restrictions;
    private Integer firstResult;
    private Integer maxResults;

    public ListRequest () {
        sorts = new ArrayList<Sort>();
        restrictions = new ArrayList<Restriction>();
        firstResult = 0;
        maxResults = DEFAULT_MAX_RESULTS;
    }

    public ListRequest (Integer firstResult, Integer maxResults) {
        this.firstResult = firstResult;
        this.maxResults = maxResults;
    }

    public List<Sort> getSorts() {
        return sorts;
    }

    public void setSorts(List<Sort> sorts) {
        this.sorts = sorts;
    }

    public void addSort(Sort sort) {
        sorts.add(sort);
    }

    public Integer getFirstResult() {

        return firstResult;
    }

    public void setFirstResult(Integer firstResult) {
        if (firstResult == null) {
            this.firstResult = 0;
        } else {
            this.firstResult = firstResult;
        }
    }

    public Integer getMaxResults() {
        return maxResults;
    }

    public void setMaxResults(Integer maxResults) {
        if (maxResults == null) {
            this.maxResults = DEFAULT_MAX_RESULTS;
        }
        this.maxResults = maxResults;
    }

    public List<Restriction> getRestrictions() {
        return restrictions;
    }

    public void setRestrictions(List<Restriction> restrictions) {
        this.restrictions = restrictions;
    }

    public void addRestriction(Restriction restriction) {
        restrictions.add(restriction);
    }

}
