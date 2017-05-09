package nl.unionsoft.common.list.worker.impl;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Order;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import nl.unionsoft.commons.converter.Converter;
import nl.unionsoft.commons.converter.ConverterWithConfig;
import nl.unionsoft.commons.converter.ListConverter;
import nl.unionsoft.common.list.model.GroupRestriction;
import nl.unionsoft.common.list.model.ListRequest;
import nl.unionsoft.common.list.model.ListResponse;
import nl.unionsoft.common.list.model.ObjectRestriction;
import nl.unionsoft.common.list.model.PropertyRestriction;
import nl.unionsoft.common.list.model.Restriction;
import nl.unionsoft.common.list.model.Restriction.Rule;
import nl.unionsoft.common.list.model.Sort;
import nl.unionsoft.common.list.model.Sort.Direction;
import nl.unionsoft.common.list.worker.ListRequestWorker;

import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.lang.StringUtils;

public class CriteriaListRequestWorkerImpl implements ListRequestWorker {

    private EntityManager entityManager;

    @Override
    public <TARGET> ListResponse<TARGET> getResults(Class<TARGET> listClass, ListRequest listRequest) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());
        listResponse.setResults(results(listClass, criteriaBuilder, listRequest));
        setCommonPropsForListResponse(listResponse, listClass, listRequest, criteriaBuilder);
        return listResponse;
    }

    @Override
    public <TARGET, SOURCE> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, Converter<TARGET, SOURCE> converter) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());
        listResponse.setResults(ListConverter.convert(converter, results(listClass, criteriaBuilder, listRequest)));
        setCommonPropsForListResponse(listResponse, listClass, listRequest, criteriaBuilder);
        return listResponse;
    }

    @Override
    public <TARGET, SOURCE, CONFIG> ListResponse<TARGET> getResults(Class<SOURCE> listClass, ListRequest listRequest, ConverterWithConfig<TARGET, SOURCE, CONFIG> converterWithConfig,
            CONFIG configObject) {
        final CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        final ListResponse<TARGET> listResponse = new ListResponse<TARGET>();
        listResponse.setSorts(listRequest.getSorts());
        listResponse.setResults(ListConverter.convert(converterWithConfig, results(listClass, criteriaBuilder, listRequest), configObject));
        setCommonPropsForListResponse(listResponse, listClass, listRequest, criteriaBuilder);
        return listResponse;
    }

    private <T> void setCommonPropsForListResponse(ListResponse<?> listResponse, Class<T> listClass, ListRequest listRequest, CriteriaBuilder criteriaBuilder) {
        listResponse.setTotalRecords(count(listClass, criteriaBuilder, listRequest));
        listResponse.setOffset(listRequest.getFirstResult());
        listResponse.setMaxResults(listRequest.getMaxResults());
    }

    private <T> List<Order> orderBys(CriteriaBuilder criteriaBuilder, Root<T> root, ListRequest listRequest) {
        List<Order> orderBy = null;
        final List<Sort> sorts = listRequest.getSorts();
        if (sorts != null && !sorts.isEmpty()) {
            orderBy = new ArrayList<Order>();
            for (final Sort sort : sorts) {
                final Path<?> path = walkPath(root, sort.getProperty());
                if (Direction.ASC.equals(sort.getDirection())) {
                    orderBy.add(criteriaBuilder.asc(path));
                } else {
                    orderBy.add(criteriaBuilder.desc(path));
                }
            }

        }
        return orderBy;
    }

    private <T> List<Predicate> restrictions(CriteriaBuilder criteriaBuilder, Root<T> root, ListRequest listRequest) {
        final List<Restriction> restrictions = listRequest.getRestrictions();
        List<Predicate> predicates = null;
        if (restrictions != null && !restrictions.isEmpty()) {
            predicates = new ArrayList<Predicate>();
            for (final Restriction restriction : restrictions) {
                final Predicate result = restriction(criteriaBuilder, restriction, root);
                if (result != null) {
                    predicates.add(result);
                }
            }
        }

        return predicates;

    }

    private <T> Predicate restriction(CriteriaBuilder criteriaBuilder, Restriction restriction, Root<T> root) {
        Predicate result = null;
        if (restriction instanceof PropertyRestriction) {
            final PropertyRestriction propertyRestriction = (PropertyRestriction) restriction;
            result = propertyRestriction(criteriaBuilder, root, restriction, propertyRestriction);
        } else if (restriction instanceof GroupRestriction) {
            final GroupRestriction groupRestriction = (GroupRestriction) restriction;
            result = groupRestriction(criteriaBuilder, root, groupRestriction);
        }
        return result;
    }

    private <T> Predicate groupRestriction(CriteriaBuilder criteriaBuilder, Root<T> root, GroupRestriction groupRestriction) {
        Predicate result = null;
        final List<Predicate> predicates = new ArrayList<Predicate>();
        for (final Restriction restriction : groupRestriction.getRestrictions()) {
            predicates.add(restriction(criteriaBuilder, restriction, root));
        }
        if (Rule.AND.equals(groupRestriction.getRule())) {
            result = criteriaBuilder.and(predicates.toArray(new Predicate[] {}));
        } else if (Rule.OR.equals(groupRestriction.getRule())) {
            result = criteriaBuilder.or(predicates.toArray(new Predicate[] {}));
        }

        return result;

    }

    private <T> Predicate propertyRestriction(CriteriaBuilder criteriaBuilder, Root<T> root, Restriction restriction, PropertyRestriction propertyRestriction) {
        Predicate result = null;

        final Path<?> path = walkPath(root, propertyRestriction.getProperty());

        if (propertyRestriction instanceof ObjectRestriction) {
            result = objectRestriction(criteriaBuilder, restriction, result, path);
        } else {
            switch (restriction.getRule()) {
                case NOT_NULL:
                    result = criteriaBuilder.isNotNull(path);
                    break;
                case NULL:
                    result = criteriaBuilder.isNull(path);
                    break;
                default:
                    break;
            }
        }
        return result;
    }

    private <T> Path<?> walkPath(Root<T> root, String path) {
        final String[] pathElements = StringUtils.split(path, ".");
        Path<Object> result = null;
        if (pathElements.length > 0) {
            result = root.get(pathElements[0]);
            if (pathElements.length > 1) {
                for (int i = 1; i < pathElements.length; i++) {
                    result = result.get(pathElements[i]);
                }
            }
        }
        return result;

    }

    private Predicate objectRestriction(CriteriaBuilder criteriaBuilder, Restriction restriction, Predicate result, Path<?> path) {
        final ObjectRestriction objectRestriction = (ObjectRestriction) restriction;
        switch (restriction.getRule()) {
            case EQ:
                result = criteriaBuilder.equal(path, objectRestriction.getObject());
                break;
            case NE:
                result = criteriaBuilder.notEqual(path, objectRestriction.getObject());
                break;
            case LIKE:
                final String restrictionValue = ObjectUtils.toString(objectRestriction.getObject());
                final StringBuilder restrictionValueBuilder = new StringBuilder(100);
                if (!StringUtils.startsWith(restrictionValue, "%")) {
                    restrictionValueBuilder.append('%');
                }
                restrictionValueBuilder.append(restrictionValue);
                if (!StringUtils.endsWith(restrictionValue, "%")) {
                    restrictionValueBuilder.append('%');
                }
                result = criteriaBuilder.like((Path<String>) path, restrictionValueBuilder.toString());
                break;
            default:
                break;
        }
        return result;
    }

    private <T> List<T> results(Class<T> listClass, CriteriaBuilder criteriaBuilder, ListRequest listRequest) {
        CriteriaQuery<T> criteriaQuery = criteriaBuilder.createQuery(listClass);
        final Root<T> root = criteriaQuery.from(listClass);
        final List<Order> orders = orderBys(criteriaBuilder, root, listRequest);
        if (orders != null) {
            criteriaQuery = criteriaQuery.orderBy(orders);
        }
        final List<Predicate> predicates = restrictions(criteriaBuilder, root, listRequest);
        if (predicates != null) {
            criteriaQuery = criteriaQuery.where(predicates.toArray(new Predicate[] {}));
        }

        final TypedQuery<T> typedQuery = entityManager.createQuery(criteriaQuery);
        if (listRequest.getMaxResults() >= 0) {
            typedQuery.setMaxResults(listRequest.getMaxResults());
        }
        typedQuery.setFirstResult(listRequest.getFirstResult());
        return typedQuery.getResultList();
    }

    private <T> Long count(Class<T> listClass, CriteriaBuilder criteriaBuilder, ListRequest listRequest) {
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        final Root<T> root = criteriaQuery.from(listClass);
        final List<Predicate> predicates = restrictions(criteriaBuilder, root, listRequest);
        if (predicates != null) {
            criteriaQuery = criteriaQuery.where(predicates.toArray(new Predicate[] {}));
        }
        criteriaQuery = criteriaQuery.select(criteriaBuilder.count(root));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }

    public EntityManager getEntityManager() {
        return entityManager;
    }

    public void setEntityManager(EntityManager entityManager) {
        this.entityManager = entityManager;
    }

}
