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

import com.google.common.collect.Lists;
import com.google.gwt.text.shared.Renderer;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.util.ModelUtils;
import fr.putnami.pwt.core.widget.client.base.AbstractInputChoice;
import fr.putnami.pwt.core.widget.client.base.AbstractInputSelect;
import fr.putnami.pwt.core.widget.client.helper.EnumRenderer;

public class EnumValuesVisitor extends AbstractVisitor {

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		Editor editor = context.getEditor();
		if (editor instanceof AbstractInputChoice) {
			ModelDriver<?> driver = context.getDriver();
			MessageHelper messageHelper = driver.getMessageHelper();
			Class<A> propertyType = null;
			Path path = context.getPath();
			Model<?> model = ModelUtils.resolveModel(driver.getModel(), path);
			if (model instanceof ModelCollection) {
				propertyType = (Class<A>) model.getLeafType();
			}
			else {
				propertyType = ModelUtils.resolveType(driver.getModel(), path);
			}

			if (ModelUtils.isEnumType(propertyType)) {
				AbstractInputChoice inputChoice = (AbstractInputChoice) editor;
				Renderer<?> renderer = new EnumRenderer(messageHelper);
				inputChoice.setItemRenderer(renderer);
				inputChoice.setItems(Lists.newArrayList(propertyType.getEnumConstants()));
				if (inputChoice instanceof AbstractInputSelect) {
					((AbstractInputSelect) inputChoice).setSelectionRenderer(renderer);

				}
			}
		}
		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
