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
package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterConstructor;

public class InjectPostconstructCreator extends InjectorCreatorDelegate
	implements InjectorWritterConstructor {

	private final JMethod postConstructMethod;

	public InjectPostconstructCreator(JMethod postConstructMethod) {
		this.postConstructMethod = postConstructMethod;
	}

	@Override
	public int getOrder() {
		return InjectorCreatorDelegate.LOWEST_PRECEDENCE;
	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s();", this.postConstructMethod.getName());
	}

}
