/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.model.client.model;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class ModelCollection<T> extends AbstractModel<Collection<T>> {

	private final Model<T> beanModel;
	private final Map<String, PropertyDescription> properties = Maps.newHashMap();

	public ModelCollection(Class<?> collectionType, Model<T> targetModel) {
		super(collectionType, targetModel != null ? targetModel.getBeanType() : null);
		this.beanModel = targetModel;
		if (targetModel != null) {
			for (String prop : targetModel.getPropertyNames()) {
				this.properties.put(prop, targetModel.getProperty(prop));
			}
		}
	}

	public ModelCollection(Class<?> targetClass, Class<T> leafClass) {
		super(targetClass, leafClass);
		this.beanModel = null;
	}

	@Override
	public <A> Model<A> getLeafModel() {
		return (Model<A>) beanModel;
	}

	@Override
	public Collection<T> newInstance() {
		return Lists.newArrayList();
	}

	public Model<T> getBeanModel() {
		return this.beanModel;
	}

	@Override
	protected Map<String, PropertyDescription> getProperties() {
		return this.properties;
	}

	@Override
	protected <P> void internalSet(Collection<T> list, String fieldName, P value) {
		if (fieldName != null && fieldName.length() > 0) {
			int index = Integer.parseInt(fieldName);
			if (list.size() > index && list instanceof List) {
				((List) list).set(index, value);
			}
			else {
				list.add((T) value);
			}
		}
	}

	@Override
	protected <P> P internalGet(Collection<T> list, String fieldName) {
		int index = Integer.parseInt(fieldName);
		return (P) Iterables.get(list, index);
	}

	@Override
	public Collection<T> cloneBean(Collection<T> beanToClone, Map<Object, Object> alreadyClonedValues) {
		if (beanToClone == null) {
			return null;
		}
		else {
			Collection<T> result = newInstance();
			for (T val : beanToClone) {
				if (beanModel != null) {
					result.add(beanModel.cloneBean(val, alreadyClonedValues));
				}
				else {
					result.add(val);
				}
			}
			return result;
		}
	}
}
