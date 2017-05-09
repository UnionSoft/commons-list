package nl.unionsoft.common.list.model;

import java.util.ArrayList;
import java.util.List;

public class GroupRestriction extends Restriction {

    private final List<Restriction> restrictions;

    public GroupRestriction (Rule rule) {
        super(rule);
        restrictions = new ArrayList<Restriction>();
    }

    public List<Restriction> getRestrictions() {
        return restrictions;
    }

}
