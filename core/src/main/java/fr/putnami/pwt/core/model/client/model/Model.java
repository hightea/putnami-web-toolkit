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

import java.util.Map;

public interface Model<T> {

	Model<?> getParentModel();

	Class<T> getBeanType();

	Class<?> getLeafType();

	T cast(Object objet);

	T newInstance();

	T cloneBean(T beanToClone);

	T cloneBean(T beanToClone, Map<Object, Object> alreadyClonedValues);

	<A> A get(Object bean, String fieldName);

	<A> boolean set(Object bean, String fieldName, A value);

	Iterable<String> getPropertyNames();

	<A> Model<A> getLeafModel();

	PropertyDescription getProperty(String fieldName);

}
