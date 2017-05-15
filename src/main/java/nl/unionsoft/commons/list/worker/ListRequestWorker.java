package nl.unionsoft.commons.list.worker;

import nl.unionsoft.commons.converter.Converter;
import nl.unionsoft.commons.converter.ConverterWithConfig;
import nl.unionsoft.commons.list.model.ListRequest;
import nl.unionsoft.commons.list.model.ListResponse;

public interface ListRequestWorker {

    public <TARGET> ListResponse<TARGET> getResults(Class<TARGET> listClass, ListRequest listRequest);

    public <TARGET, SOURCE> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, final Converter<TARGET, SOURCE> converter);

    public <TARGET, SOURCE, CONFIG> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, final ConverterWithConfig<TARGET, SOURCE, CONFIG> converterWithConfig,
            CONFIG configObject);

}
