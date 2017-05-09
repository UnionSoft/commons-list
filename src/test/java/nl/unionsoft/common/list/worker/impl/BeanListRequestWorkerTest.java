package nl.unionsoft.common.list.worker.impl;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;
import nl.unionsoft.common.list.dto.SomeDto;
import nl.unionsoft.common.list.dto.SomeOtherDto;
import nl.unionsoft.common.list.model.ListRequest;
import nl.unionsoft.common.list.model.ListResponse;
import nl.unionsoft.common.list.model.ObjectRestriction;
import nl.unionsoft.common.list.model.Restriction.Rule;
import nl.unionsoft.common.list.model.Sort;
import nl.unionsoft.common.list.model.Sort.Direction;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class BeanListRequestWorkerTest {

    private BeanListRequestWorkerImpl beanListRequestWorkerImpl;

    @Before
    public void before() {

        final List<SomeDto> someDtos = new ArrayList<SomeDto>();
        someDtos.add(new SomeDto("1", "hello", null));
        someDtos.add(new SomeDto("2", "world", null));
        someDtos.add(new SomeDto("3", "world", null));
        someDtos.add(new SomeDto("4", "test", new SomeOtherDto("11", "blaat")));
        beanListRequestWorkerImpl = new BeanListRequestWorkerImpl() {

            @Override
            public <TARGET> List<TARGET> fetchData(Class<TARGET> listClass, ListRequest listRequest) {
                return (List<TARGET>) someDtos;
            }
        };
    }

    @Test
    public void test() {
        final ListRequest listRequest = new ListRequest();
        listRequest.addRestriction(new ObjectRestriction(Rule.EQ, "name", "world"));
        listRequest.addSort(new Sort("id", Direction.DESC));
        final ListResponse<SomeDto> listResponse = beanListRequestWorkerImpl.getResults(SomeDto.class, listRequest);
        final List<SomeDto> results = listResponse.getResults();
        Assert.assertEquals(2, results.size());
        {
            final SomeDto someDto = results.get(0);
            Assert.assertEquals("3", someDto.getId());

        }
        {
            final SomeDto someDto = results.get(1);
            Assert.assertEquals("2", someDto.getId());
        }
    }

    @Test
    @Ignore
    public void testChildBean() {
        final ListRequest listRequest = new ListRequest();
        listRequest.addRestriction(new ObjectRestriction(Rule.EQ, "someOtherDto.name", "blaat"));
        final ListResponse<SomeDto> listResponse = beanListRequestWorkerImpl.getResults(SomeDto.class, listRequest);
        final List<SomeDto> results = listResponse.getResults();
        Assert.assertEquals(1, results.size());
        {
            final SomeDto someDto = results.get(0);
            Assert.assertEquals("4", someDto.getId());

        }
    }
}
