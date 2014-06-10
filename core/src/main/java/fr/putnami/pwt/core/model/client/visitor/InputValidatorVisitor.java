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
package fr.putnami.pwt.core.model.client.visitor;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class InputValidatorVisitor extends AbstractVisitor {

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		B editor = context.getEditor();
		Path path = context.getPath();
		if (path != null && editor instanceof EditorInput) {
			EditorInput editorInput = (EditorInput) editor;
			ModelDriver<?> driver = context.getDriver();
			Collection<Validator<A>> validators = ModelUtils.resolveValidators(driver.getModel(), path);
			for (Validator<A> validator : validators) {
				editorInput.addValidator(validator);
			}
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
