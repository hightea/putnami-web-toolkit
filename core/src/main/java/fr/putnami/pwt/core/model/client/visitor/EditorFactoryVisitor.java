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

import com.google.common.collect.Maps;

import java.util.Map;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorCollection;
import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.editor.client.factory.InputFactory;
import fr.putnami.pwt.core.editor.client.factory.OutputFactory;
import fr.putnami.pwt.core.editor.client.visitor.AbstractVisitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.EditorProvider;
import fr.putnami.pwt.core.model.client.base.HasEditorProvider;
import fr.putnami.pwt.core.model.client.base.HasInputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasOutputEditorFactory;
import fr.putnami.pwt.core.model.client.base.HasWidgetFactory;
import fr.putnami.pwt.core.model.client.factory.ContextFactory;
import fr.putnami.pwt.core.model.client.factory.EditorFactoryManager;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.client.model.ModelCollection;
import fr.putnami.pwt.core.model.client.util.ModelUtils;

public class EditorFactoryVisitor extends AbstractVisitor {

  private static class InternalEditorProvider implements EditorProvider {
    final Context<?> parentContext;
    final Class propertyType;

    CloneableWidget inputFactory;
    CloneableWidget outputFactory;

    private Map<Integer, Editor> inputEditors;
    private Map<Integer, Editor> outputEditors;

    private InternalEditorProvider(
        Context<?> parentContext, Class propertyType,
        CloneableWidget inputFactory, CloneableWidget outputFactory) {
      super();
      this.parentContext = parentContext;
      this.propertyType = propertyType;
      this.inputFactory = inputFactory;
      this.outputFactory = outputFactory;
    }

    @Override
    public <E extends Editor> E getEditor(Boolean readonly) {
      return ensureEditor(readonly, null);
    }

    @Override
    public <E extends Editor> E getEditorForTraversal(Boolean readonly, Integer index) {
      return ensureEditor(readonly, index);
    }

    private <E extends Editor> E ensureEditor(Boolean readonly, Integer index) {
      if (!Boolean.FALSE.equals(readonly)) {
        if (outputEditors == null) {
          outputEditors = Maps.newHashMap();
        }
        Editor editor = outputEditors.get(index);
        if (editor == null) {
          editor = (Editor) getOutputFacoty().cloneWidget();
          outputEditors.put(index, editor);
          initEditor(editor, index);
        }
        return (E) editor;
      }
      if (inputEditors == null) {
        inputEditors = Maps.newHashMap();
      }
      Editor editor = inputEditors.get(index);
      if (editor == null) {
        editor = (Editor) getInputFacoty().cloneWidget();
        inputEditors.put(index, editor);
        initEditor(editor, index);
      }
      return (E) editor;
    }

    private CloneableWidget getOutputFacoty() {
      if (outputFactory == null) {
        outputFactory = EditorFactoryManager.get().createOutputForType(propertyType, parentContext);
      }
      assert outputFactory != null : "output factory is null, can not create an output editor for " + propertyType;
      return outputFactory;
    }

    private CloneableWidget getInputFacoty() {
      if (inputFactory == null) {
        inputFactory = EditorFactoryManager.get().createInputForType(propertyType, parentContext);
      }
      assert inputFactory != null : "intput factory is null, can not create an input editor for " + propertyType;
      return inputFactory;
    }

    void initEditor(Editor editor, Integer index) {
      if (index != null) {
        String path = "[" + index + "]";
        editor.setPath(path);
      }
      ModelDriver<?> driver = parentContext.getDriver();
      Context<?> context = ContextFactory.Util.get().createContext(driver, parentContext, editor);
      driver.accept(new BinderVisitor(driver, driver.getValue()), context);
    }
  }

  public static interface IndexedEditorFactory {

    EditorInput newInput(Integer index);

    EditorOutput newOutput(Integer index);
  }

  @Override
  public <A, B extends Editor> boolean visit(Context<B> context) {
    Editor editor = context.getEditor();
    if (editor instanceof HasEditorProvider) {
      Path path = context.getPath();
      Class<A> propertyType = null;
      ModelDriver<?> driver = context.getDriver();
      Model<?> model = ModelUtils.resolveModel(driver.getModel(), path);
      if (model instanceof ModelCollection &&
          (editor instanceof EditorCollection || path.get(path.size() - 1).getIndexKey() != null)) {
        propertyType = (Class<A>) model.getLeafType();
      }
      else {
        propertyType = ModelUtils.resolveType(driver.getModel(), path);
      }

      CloneableWidget widgetFactory = null;
      InputFactory inputFactory = null;
      OutputFactory outputFactory = null;

      if (editor instanceof HasWidgetFactory) {
        widgetFactory = ((HasWidgetFactory) editor).getWidgetFactory();
      }

      if (editor instanceof HasInputEditorFactory) {
        inputFactory = ((HasInputEditorFactory) editor).getInputFactory();
      }

      if (editor instanceof HasOutputEditorFactory) {
        outputFactory = ((HasOutputEditorFactory) editor).getOutputFactory();
      }

      EditorProvider provider = new InternalEditorProvider(context, propertyType,
          inputFactory == null ? widgetFactory : inputFactory,
              outputFactory == null ? widgetFactory : outputFactory
          );

      ((HasEditorProvider) editor).setEditorProvider(provider);

    }

    return true;
  }

  @Override
  public VisitorTrigger trigerOn() {
    return VisitorTrigger.INITALIZE;
  }

}
