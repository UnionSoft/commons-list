package nl.unionsoft.common.list.model;

public class ObjectRestriction extends PropertyRestriction {

    public ObjectRestriction (Rule rule, String property, Object object) {
        super(rule, property);
        this.object = object;

    }

    private Object object;

    public Object getObject() {
        return object;
    }

    public void setObject(Object object) {
        this.object = object;
    }

}
