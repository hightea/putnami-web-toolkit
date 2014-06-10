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
package fr.putnami.pwt.core.model.client.factory;

import java.util.Map;

import com.google.common.collect.Maps;
import com.google.gwt.core.shared.GWT;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.factory.InputFactory;
import fr.putnami.pwt.core.editor.client.factory.LabelFactory;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.editor.client.factory.TooltipFactory;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class EditorFactoryManager {

	private static EditorFactoryManager INSTANCE;

	public static final EditorFactoryManager get() {
		if (INSTANCE == null) {
			INSTANCE = GWT.create(EditorFactoryManager.class);
		}
		return INSTANCE;
	}

	private final Map<Class<?>, InputFactory> inputFactories = Maps.newHashMap();
	private final Map<Class<?>, OutputFactory> outputFactories = Maps.newHashMap();

	private LabelFactory labelFactory;
	private TooltipFactory tooltipFactory;

	public void registerInputFactory(Class<?> propertyType, InputFactory factory) {
		inputFactories.put(propertyType, factory);
	}

	public void registerOutputFactory(Class<?> propertyType, OutputFactory factory) {
		outputFactories.put(propertyType, factory);
	}

	public void setLabelFactory(LabelFactory labelFactory) {
		this.labelFactory = labelFactory;
	}

	public void setTooltipFactory(TooltipFactory tooltipFactory) {
		this.tooltipFactory = tooltipFactory;
	}

	public <A, B extends Editor> EditorInput<A> createInputForType(Class<?> propertyType, Context<B> context) {
		InputFactory factory = inputFactories.get(propertyType);
		if (factory == null) {
			for (Class<?> parentClass : ModelUtils.getTypeHierachy(propertyType)) {
				factory = inputFactories.get(parentClass);
				if (factory != null) {
					break;
				}
			}
		}
		if (factory == null) {
			return null;
		}

		return (EditorInput<A>) factory.cloneWidget();
	}

	public <A, B extends Editor> EditorOutput<A> createOutputForType(Class<?> propertyType, Context<B> context) {
		OutputFactory factory = outputFactories.get(propertyType);
		if (factory == null) {
			for (Class<?> parentClass : ModelUtils.getTypeHierachy(propertyType)) {
				factory = outputFactories.get(parentClass);
				if (factory != null) {
					break;
				}
			}
		}
		if (factory == null) {
			return null;
		}

		return (EditorOutput<A>) factory.cloneWidget();
	}

	public <A, B extends Editor> EditorLabel createLabel() {
		EditorLabel label = null;
		if (labelFactory != null) {
			label = labelFactory.newLabel();
		}
		return label;
	}

	public <A, B extends Editor> EditorLabel createTooltip(IsWidget target, String tooltipMessage) {
		EditorLabel tooltip = null;
		if (tooltipFactory != null) {
			tooltip = tooltipFactory.newTooltip(target, tooltipMessage);
		}
		return tooltip;
	}
}
