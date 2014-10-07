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
package fr.putnami.pwt.core.model.client.factory;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.model.client.ModelDriver;

public interface ContextFactory {

	public static class Util {
		private static ContextFactory instance;

		public static ContextFactory get() {
			if (Util.instance == null) {
				Util.instance = GWT.create(ContextFactory.class);
			}
			return Util.instance;
		}
	}

	<B extends Editor> Context<B> createContext(ModelDriver<?> driver, Context<?> parentContext,
		B editor);
}
