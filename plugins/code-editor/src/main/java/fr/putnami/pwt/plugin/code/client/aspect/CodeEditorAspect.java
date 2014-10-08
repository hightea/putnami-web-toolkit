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
package fr.putnami.pwt.plugin.code.client.aspect;

import com.google.common.base.Predicate;

import java.util.List;

import fr.putnami.pwt.plugin.code.client.base.CodeEditorDriver;

public interface CodeEditorAspect {

	enum AspectTrigger implements Predicate<CodeEditorAspect> {
			INITALIZE,
			EDIT,
			FLUSH,
			MANUAL,
			CHANGE;

		@Override
		public boolean apply(CodeEditorAspect input) {
			return input.trigerOn().contains(this);
		}
	}

	List<AspectTrigger> trigerOn();

	void apply(CodeEditorDriver editorDriver);

	CodeEditorAspect copy();

}
