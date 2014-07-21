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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorError;
import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.EditorValue;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.editor.client.factory.InputFactory;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.model.client.base.EditorProvider;
import fr.putnami.pwt.core.model.client.base.HasEditorProvider;
import fr.putnami.pwt.core.model.client.base.HasInputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasLabelEditor;
import fr.putnami.pwt.core.model.client.base.HasOutputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.model.client.base.HasWidgetFactory;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractForm.Layout;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.HasFormType;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class FormGroup<T> extends AbstractPanel implements
HasFormType,
CloneableWidget,
EditorValue<T>,
HasLabelEditor,
HasEditorProvider,
HasWidgetFactory,
HasOutputEditorFactory<T>,
HasInputEditorFactory<T>,
HasReadonly,
EditorLabel,
EditorError
{

	private static final CssStyle STYLE_FORM_GROUP = new SimpleStyle("form-group");
	private static final CssStyle STYLE_ERROR = new SimpleStyle("has-error");
	private static final CssStyle STYLE_SCREAN_READER = new SimpleStyle("sr-only");
	private static final CssStyle STYLE_WARNING = new SimpleStyle("help-warning");
	private static final CssStyle STYLE_SUCCESS = new SimpleStyle("help-succes");

	private Layout type;

	private InputFactory inputFactory;
	private OutputFactory outputFactory;
	private CloneableWidget widgetFactory;

	private EditorProvider editorProvider;

	private EditorLabel label;
	private Help help;
	private ErrorGroup error;

	private Boolean readonly;
	private T value;

	public FormGroup() {
		super(DivElement.TAG);
		StyleUtils.addStyle(this, STYLE_FORM_GROUP);
	}

	protected FormGroup(FormGroup<T> source) {
		super(source);
		type = source.type;
		inputFactory = source.inputFactory;
		outputFactory = source.outputFactory;
		widgetFactory = source.widgetFactory;
		label = WidgetUtils.cloneWidget(source.label);
		help = WidgetUtils.cloneWidget(source.help);
		error = WidgetUtils.cloneWidget(source.error);
		readonly = source.readonly;
	}

	@Override
	public IsWidget cloneWidget() {
		return new FormGroup<T>(this);
	}

	@Override
	public Boolean getReadonly() {
		return readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	@Override
	public CloneableWidget getWidgetFactory() {
		return widgetFactory;
	}

	@Override
	public InputFactory getInputFactory() {
		return inputFactory;
	}

	@Override
	public OutputFactory getOutputFactory() {
		return outputFactory;
	}

	@Override
	public EditorLabel getLabelEditor() {
		return label;
	}

	@Override
	public void setLabelEditor(EditorLabel editor) {
		this.label = editor;
	}

	@Override
	public void setEditorProvider(EditorProvider provider) {
		this.editorProvider = provider;
	}

	@Override
	public Layout getLayout() {
		return type;
	}

	@Override
	public void setLayout(Layout type) {
		this.type = type;
	}

	@Override
	public void add(IsWidget child) {
		if (child instanceof InputFactory) {
			assert this.inputFactory == null : "inputFactory may only be set once";
			this.inputFactory = (InputFactory) child;
		}
		if (child instanceof OutputFactory) {
			assert this.outputFactory == null : "outputFactory may only be set once";
			this.outputFactory = (OutputFactory) child;
		}

		if (child instanceof Label) {
			label = (Label) child;
			addEditor(child);
		}
		else if (child instanceof Help) {
			this.help = (Help) child;
		}
		else if (this.inputFactory == null
				&& this.outputFactory == null
				&& child instanceof CloneableWidget) {
			this.widgetFactory = (CloneableWidget) child;
		}
	}

	@Override
	public boolean isLabelMandatory() {
		return false;
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {
				EditorLabel.HELP_SUFFIX
		};
	}

	@Override
	public String getText() {
		if (help == null) {
			return null;
		}
		else {
			return help.getText();
		}
	}

	@Override
	public void setText(String message) {
		if (help == null) {
			help = new Help();
		}
		help.setText(message);
	}

	@Override
	public void clearErrors() {
		if (error != null) {
			error.clearErrors();
			error = null;
			StyleUtils.removeStyle(this, STYLE_ERROR);
		}
	}

	@Override
	public void displayErrors(Iterable<Error> errors) {
		if (error == null) {
			error = new ErrorGroup();
		}
		error.displayErrors(errors);
		error.redraw();
		StyleUtils.toggleStyle(this, STYLE_ERROR, !Boolean.TRUE.equals(readonly) && error != null && error.hasError());
	}

	@Override
	public void redraw() {
		clear();

		StyleUtils.toggleStyle(asWidgetOrNull(label), STYLE_SCREAN_READER, type == Layout.INLINE);
		addIfNotNull(label, 3, 0, false);
		Editor editor = editorProvider.getEditor(readonly);
		if (!Boolean.FALSE.equals(readonly)) {
			addIfNotNull(editor, 9, 0, true);
		}
		else {
			addIfNotNull(editor, 9, 0, true);
			addIfNotNull(error, 9, 3, true);
			addIfNotNull(help, 9, 3, true);
		}
	}

	private void addIfNotNull(Editor e, int size, int offset, boolean wrap) {
		if (e instanceof IsWidget) {
			boolean wrapInCol = wrap;
			wrapInCol &= type == Layout.HORIZONTAL;

			Widget toAdd = asWidgetOrNull((IsWidget) e);
			if (wrapInCol) {
				GridColumn column = new GridColumn();
				column.add(toAdd);
				column.setSize(size);
				column.setOffset(offset);
				toAdd = column;
			}
			if (type == Layout.HORIZONTAL) {
				if (size > 0) {
					StyleUtils.addStyle(toAdd, new GridColumn.SizeStyle(GridColumn.PREFIX_SIZE_MD, size));
				}
				if (offset > 0) {
					StyleUtils.addStyle(toAdd, new GridColumn.OffsetStyle(GridColumn.PREFIX_OFFSET_MD, offset));
				}
			}
			append(toAdd);
		}

	}

	@Override
	public T getValue() {
		return this.value;
	}

	@Override
	public void edit(T value) {
		this.value = value;
	}

}
