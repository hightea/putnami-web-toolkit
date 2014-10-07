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
package fr.putnami.pwt.core.model.client.visitor;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.EditorModel;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class ModelInitializerVisitor extends AbstractVisitor {

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		Editor editor = context.getEditor();
		if (editor instanceof EditorModel && ((EditorModel) editor).getModel() == null) {
			EditorModel editorModel = (EditorModel) editor;
			ModelDriver<?> driver = context.getDriver();

			Model<?> model = ModelUtils.resolveModel(driver.getModel(), context.getPath());
			Model<?> toInitialize = model.getLeafModel() != null ? model.getLeafModel() : model;

			editorModel.setMessageHelper(driver.getMessageHelper());
			editorModel.initialize(toInitialize, driver.getVisitors().toArray(
				new Visitor[driver.getVisitors().size()]));
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
