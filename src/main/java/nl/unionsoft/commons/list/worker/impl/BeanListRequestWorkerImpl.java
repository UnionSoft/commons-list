package nl.unionsoft.commons.list.worker.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import nl.unionsoft.commons.converter.Converter;
import nl.unionsoft.commons.converter.ConverterWithConfig;
import nl.unionsoft.commons.converter.ListConverter;
import nl.unionsoft.commons.list.model.GroupRestriction;
import nl.unionsoft.commons.list.model.ListRequest;
import nl.unionsoft.commons.list.model.ListResponse;
import nl.unionsoft.commons.list.model.ObjectRestriction;
import nl.unionsoft.commons.list.model.PropertyRestriction;
import nl.unionsoft.commons.list.model.Restriction;
import nl.unionsoft.commons.list.model.Sort;
import nl.unionsoft.commons.list.model.Restriction.Rule;
import nl.unionsoft.commons.list.model.Sort.Direction;
import nl.unionsoft.commons.list.worker.ListRequestWorker;

import org.apache.commons.beanutils.BeanComparator;
import org.apache.commons.beanutils.BeanPredicate;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.PredicateUtils;
import org.apache.commons.collections.comparators.ComparableComparator;
import org.apache.commons.collections.comparators.ComparatorChain;
import org.apache.commons.collections.comparators.NullComparator;
import org.apache.commons.collections.comparators.ReverseComparator;
import org.apache.commons.collections.functors.AllPredicate;
import org.apache.commons.collections.functors.AnyPredicate;
import org.apache.commons.collections.functors.EqualPredicate;
import org.apache.commons.collections.functors.NotPredicate;

public abstract class BeanListRequestWorkerImpl implements ListRequestWorker {

    @Override
    public <TARGET> ListResponse<TARGET> getResults(Class<TARGET> listClass, ListRequest listRequest) {
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());

        final List<TARGET> unfilteredData = fetchData(listClass, listRequest);
        listResponse.setMaxResults(unfilteredData.size());

        final List<TARGET> data = handleData(listRequest, unfilteredData);
        listResponse.setTotalRecords(data.size());
        listResponse.setResults(data);
        return listResponse;
    }

    @Override
    public <TARGET, SOURCE> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, Converter<TARGET, SOURCE> converter) {
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());

        final List<SOURCE> unfilteredData = fetchData(listClass, listRequest);
        listResponse.setMaxResults(unfilteredData.size());

        final List<TARGET> data = ListConverter.convert(converter, handleData(listRequest, unfilteredData));
        listResponse.setTotalRecords(data.size());
        listResponse.setResults(data);
        return listResponse;
    }

    @Override
    public <TARGET, SOURCE, CONFIG> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, ConverterWithConfig<TARGET, SOURCE, CONFIG> converterWithConfig,
            CONFIG configObject) {
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());

        final List<SOURCE> unfilteredData = fetchData(listClass, listRequest);
        listResponse.setMaxResults(unfilteredData.size());

        final List<TARGET> data = ListConverter.convert(converterWithConfig, handleData(listRequest, unfilteredData), configObject);
        listResponse.setTotalRecords(data.size());
        listResponse.setResults(data);
        return listResponse;
    }

    private <TARGET> List<TARGET> handleData(ListRequest listRequest, List<TARGET> data) {
        restrictions(data, listRequest.getRestrictions());
        sorts(data, listRequest.getSorts());
        return data;
    }

    @SuppressWarnings("unchecked")
    public void sorts(final List<?> data, List<Sort> sorts) {
        final ComparatorChain comparatorChain = new ComparatorChain();

        for (final Sort sort : sorts) {
            Comparator<?> orderDirectionComparator = null;
            if (Direction.DESC.equals(sort.getDirection())) {
                // If DESC, then use the reverseComparator...
                orderDirectionComparator = new ReverseComparator();
            } else {
                // Else use the default ComparableComparator. Note this is not null since
                // we always use a NullComparator!
                orderDirectionComparator = ComparableComparator.getInstance();
            }
            comparatorChain.addComparator(new BeanComparator(sort.getProperty(), new NullComparator(orderDirectionComparator)));
        }
        if (comparatorChain.size() > 0) {
            Collections.sort(data, comparatorChain);
        }
    }

    public void restrictions(final List<?> data, List<Restriction> restrictions) {
        final List<Predicate> predicates = new ArrayList<Predicate>();
        for (final Restriction restriction : restrictions) {
            final Predicate result = restriction(restriction);
            if (result != null) {
                predicates.add(result);
            }
        }

        if (!predicates.isEmpty()) {
            if (predicates.size() == 1) {
                CollectionUtils.filter(data, predicates.get(0));
            } else {
                CollectionUtils.filter(data, PredicateUtils.allPredicate(predicates));
            }

        }
    }

    public void restriction(final List<?> data, Restriction restriction) {
        final List<Predicate> predicates = new ArrayList<Predicate>();
        final Predicate result = restriction(restriction);
        if (result != null) {
            CollectionUtils.filter(data, predicates.get(0));
        }
    }

    public Predicate restriction(Restriction restriction) {
        Predicate result = null;
        if (restriction instanceof PropertyRestriction) {
            result = handlePropertyRestriction((PropertyRestriction) restriction);
        } else if (restriction instanceof GroupRestriction) {
            final GroupRestriction groupRestriction = (GroupRestriction) restriction;
            result = groupRestriction(groupRestriction);
        }
        return result;
    }

    private Predicate groupRestriction(GroupRestriction groupRestriction) {

        Predicate result = null;
        final List<Predicate> predicates = new ArrayList<Predicate>();
        for (final Restriction restriction : groupRestriction.getRestrictions()) {
            predicates.add(restriction(restriction));
        }

        if (Rule.AND.equals(groupRestriction.getRule())) {
            result = new AllPredicate(predicates.toArray(new Predicate[] {}));
        } else if (Rule.OR.equals(groupRestriction.getRule())) {
            result = new AnyPredicate(predicates.toArray(new Predicate[] {}));
        }

        return result;
    }

    private Predicate handlePropertyRestriction(PropertyRestriction propertyRestriction) {
        final String property = propertyRestriction.getProperty();
        Predicate result = null;
        if (propertyRestriction instanceof ObjectRestriction) {
            result = handleObjectRestriction((ObjectRestriction) propertyRestriction);
        } else {
            switch (propertyRestriction.getRule()) {
                case NOT_NULL:
                    result = new BeanPredicate(property, PredicateUtils.notNullPredicate());
                    break;
                case NULL:
                    result = new BeanPredicate(property, PredicateUtils.nullPredicate());
                    break;
            }
        }
        return result;
    }

    private Predicate handleObjectRestriction(ObjectRestriction objectRestriction) {
        Predicate result = null;
        final String property = objectRestriction.getProperty();
        final Object object = objectRestriction.getObject();
        switch (objectRestriction.getRule()) {
            case EQ:
                result = new BeanPredicate(property, new EqualPredicate(object));
                break;
            case NE:
                result = new BeanPredicate(property, new NotPredicate(new EqualPredicate(object)));
                break;
        }
        return result;
    }

    public abstract <TARGET> List<TARGET> fetchData(Class<TARGET> listClass, ListRequest listRequest);

}
