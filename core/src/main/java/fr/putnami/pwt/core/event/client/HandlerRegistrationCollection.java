/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.event.client;

import com.google.common.collect.Lists;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.Collection;

/**
 * A utility class to help deal with {@link HandlerRegistration handler registrations}.
 *
 * @since 1.0
 */
public class HandlerRegistrationCollection implements com.google.gwt.event.shared.HandlerRegistration {

	private final Collection<HandlerRegistration> handlers = Lists.newArrayList();

	/**
	 * Instantiates a new handler registration collection.
	 */
	public HandlerRegistrationCollection() {
	}

	/**
	 * Adds on handler registration to the collection.
	 *
	 * @param handler the handler
	 */
	public void add(HandlerRegistration handler) {
		this.handlers.add(handler);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.google.web.bindery.event.shared.HandlerRegistration#removeHandler()
	 */
	@Override
	public void removeHandler() {
		for (HandlerRegistration handler : this.handlers) {
			handler.removeHandler();
		}
		this.handlers.clear();
	}

	/**
	 * Size of the collection.
	 *
	 * @return the int
	 */
	public int size() {
		return this.handlers.size();
	}

}
