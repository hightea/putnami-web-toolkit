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
package fr.putnami.pwt.core.editor.client;

import fr.putnami.pwt.core.editor.client.aspect.ContextAspect;

public interface Context<E extends Editor> {

	boolean isRootContext();

	<D extends Driver<?>> D getDriver();

	Context<?> getParentContext();

	Path getPath();

	E getEditor();

	<A extends ContextAspect> boolean hasAspect(Class<A> contextType);

	<A extends ContextAspect> A getAspect(Class<A> contextType);

	<A extends ContextAspect> void addAspect(A contextAspect);

}
