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
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class PlaceholderRendererVisitor extends AbstractVisitor {

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		Editor editor = context.getEditor();
		if (editor instanceof HasPlaceholder) {
			ModelDriver<?> driver = context.getDriver();
			MessageHelper messageHelper = driver.getMessageHelper();
			Class<?> parentPropertyType = null;
			Path path = context.getPath();
			Path parentPath = path.subPath(0, path.size() - 1);
			Model<?> model = ModelUtils.resolveModel(driver.getModel(), parentPath);
			if (model instanceof ModelCollection) {
				parentPropertyType = model.getLeafType();
			} else {
				parentPropertyType = ModelUtils.resolveType(driver.getModel(), parentPath);
			}

			HasPlaceholder hasPlaceholder = (HasPlaceholder) editor;
			String defaulText = hasPlaceholder.getPlaceholder();
			if (messageHelper != null && defaulText == null) {
				String label =
					messageHelper.getMessage(parentPropertyType, path, HasPlaceholder.PLACEHOLDER_SUFFIX);
				if (label != null) {
					hasPlaceholder.setPlaceholder(label);
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
