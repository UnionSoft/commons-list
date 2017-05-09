package nl.unionsoft.common.list.model;

public class PropertyRestriction extends Restriction {
    public PropertyRestriction (Rule rule, String property) {
        super(rule);
        this.property = property;
    }

    private String property;

    public String getProperty() {
        return property;
    }

    public void setProperty(String property) {
        this.property = property;
    }

}
