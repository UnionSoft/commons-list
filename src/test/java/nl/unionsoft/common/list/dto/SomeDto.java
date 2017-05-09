package nl.unionsoft.common.list.dto;

public class SomeDto {

    private final String id;
    private final String name;
    private final SomeOtherDto someOtherDto;

    public SomeDto (String id, String name, SomeOtherDto someOtherDto) {
        this.id = id;
        this.name = name;
        this.someOtherDto = someOtherDto;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public SomeOtherDto getSomeOtherDto() {
        return someOtherDto;
    }

    @Override
    public String toString() {
        return "SomeDto [id=" + id + ", name=" + name + "]";
    }

}
