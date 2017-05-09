package nl.unionsoft.common.list.worker;

import nl.unionsoft.common.list.model.ListRequest;
import nl.unionsoft.common.list.model.ListResponse;
import nl.unionsoft.commons.converter.Converter;
import nl.unionsoft.commons.converter.ConverterWithConfig;

public interface ListRequestWorker {

    public <TARGET> ListResponse<TARGET> getResults(Class<TARGET> listClass, ListRequest listRequest);

    public <TARGET, SOURCE> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, final Converter<TARGET, SOURCE> converter);

    public <TARGET, SOURCE, CONFIG> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, final ConverterWithConfig<TARGET, SOURCE, CONFIG> converterWithConfig,
            CONFIG configObject);

}
