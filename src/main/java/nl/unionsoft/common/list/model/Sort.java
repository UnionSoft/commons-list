package nl.unionsoft.common.list.model;

import org.apache.commons.lang.StringUtils;

public class Sort {

    private String property;
    private Direction direction;

    public Sort (String property, Direction direction) {
        this.property = property;
        this.direction = direction;
    }

    public Sort (String propertyAndDirection) {
        final String[] keys = StringUtils.split(propertyAndDirection, ",| ;:-");
        if (keys.length != 2) {
            throw new IllegalArgumentException("propertyAndDirection '" + propertyAndDirection + "' are invalid!");
        }

        property = keys[0];
        direction = Direction.valueOf(StringUtils.upperCase(keys[1]));

    }

    public String getProperty() {
        return property;
    }

    public void setProperty(String property) {
        this.property = property;
    }

    public Direction getDirection() {
        return direction;
    }

    public void setDirection(Direction direction) {
        this.direction = direction;
    }

    public String getStringValue() {
        final StringBuilder valueBuilder = new StringBuilder();
        valueBuilder.append(property);
        valueBuilder.append('|');
        valueBuilder.append(direction.toString());
        return valueBuilder.toString();
    }

    public enum Direction {
        ASC, DESC;

    }
}
