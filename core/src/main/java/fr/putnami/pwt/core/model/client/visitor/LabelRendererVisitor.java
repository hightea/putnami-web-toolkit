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
import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class LabelRendererVisitor extends AbstractVisitor {

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		Editor editor = context.getEditor();
		ModelDriver<?> driver = context.getDriver();
		MessageHelper messageHelper = driver.getMessageHelper();
		if (messageHelper != null && editor instanceof EditorLabel) {
			EditorLabel editorLabel = (EditorLabel) editor;

			Class<?> parentPropertyType = null;
			Path path = context.getPath();
			Path parentPath = path.subPath(0, path.size() - 1);
			Model<?> model = ModelUtils.resolveModel(driver.getModel(), parentPath);
			if (model instanceof ModelCollection) {
				parentPropertyType = model.getLeafType();
			} else {
				parentPropertyType = ModelUtils.resolveType(driver.getModel(), parentPath);
			}

			String defaulText = editorLabel.getText();
			String label = defaulText;
			String labelKey = editorLabel.getLabelKey();
			String firstKey = null;
			if (label == null) {
				for (String suffix : editorLabel.getSuffix()) {
					String key = null;
					if (labelKey == null) {
						key = messageHelper.getMessageKey(path, suffix);
						label = messageHelper.getMessage(parentPropertyType, path, suffix);
					} else {
						key = labelKey + suffix;
						label = messageHelper.findMessage(parentPropertyType, key);
					}
					if (firstKey == null) {
						firstKey = key;
					}
					if (label != null) {
						break;
					}
				}
			}

			if (label == null && editorLabel.isLabelMandatory()) {
				label = messageHelper.createDefaultMessage(firstKey);
			}
			if (label != null) {
				editorLabel.setText(label);
			}
		}

		return true;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.INITALIZE;
	}

}
